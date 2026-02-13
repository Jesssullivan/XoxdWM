## NixOS full-stack integration test: compositor + Emacs + home-manager + IPC.
##
## Run via: nix build .#checks.x86_64-linux.full-stack-test
## Requires KVM (QEMU VM-based test).

{ pkgs, self, home-manager, ... }:

let
  ipcHelper = pkgs.writeScript "ipc-helper" (builtins.readFile ./ipc-helper.py);
  compositorPkg = self.packages.${pkgs.system}.compositor-headless;
  elispPkg = self.packages.${pkgs.system}.ewwm-elisp;
in
pkgs.testers.nixosTest {
  name = "exwm-vr-full-stack-test";

  nodes.machine = { config, pkgs, ... }: {
    imports = [
      self.nixosModules.exwm-vr
      home-manager.nixosModules.home-manager
    ];

    # Headless compositor
    services.exwm-vr = {
      enable = true;
      compositor.package = compositorPkg;
      compositor.extraArgs = [ "--backend" "headless" ];
    };

    # Test user with home-manager config
    users.users.testuser = {
      isNormalUser = true;
      extraGroups = [ "ewwm" "video" "input" ];
    };

    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.testuser = { config, pkgs, ... }: {
      imports = [
        self.homeManagerModules.exwm-vr
      ];

      programs.exwm-vr = {
        enable = true;
        theme = "modus-vivendi";
        config = {
          ewwm_workspace_number = 4;
          ewwm_floating_border_width = 2;
          ewwm_vr_enable = false;
        };
      };

      home.stateVersion = "24.05";
    };

    # Override compositor service for test (headless VM has no graphical-session)
    systemd.user.services.ewwm-compositor = {
      wantedBy = pkgs.lib.mkForce [ "default.target" ];
      serviceConfig = {
        ExecStart = pkgs.lib.mkForce
          "${compositorPkg}/bin/ewwm-compositor --backend headless";
        Restart = pkgs.lib.mkForce "no";
        ProtectHome = pkgs.lib.mkForce false;
        SupplementaryGroups = pkgs.lib.mkForce [ ];
      };
    };

    # Emacs user service (override for headless VM)
    systemd.user.services.ewwm-emacs = {
      wantedBy = pkgs.lib.mkForce [ "default.target" ];
      serviceConfig = {
        ExecStart = pkgs.lib.mkForce (builtins.concatStringsSep " " [
          "${pkgs.emacs-nox}/bin/emacs" "--fg-daemon"
          "-L" "${elispPkg}/share/emacs/site-lisp/ewwm/core"
          "-L" "${elispPkg}/share/emacs/site-lisp/ewwm/vr"
          "-L" "${elispPkg}/share/emacs/site-lisp/ewwm/ext"
        ]);
        Restart = pkgs.lib.mkForce "no";
        ProtectHome = pkgs.lib.mkForce false;
        Type = pkgs.lib.mkForce "simple";
      };
    };

    # Enable lingering so user services start without login
    users.users.testuser.linger = true;

    # IPC helper + python
    environment.etc."ipc-helper.py".source = ipcHelper;
    environment.systemPackages = with pkgs; [ python3Minimal emacs-nox ];

    # VM resources
    virtualisation.memorySize = 2048;
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("multi-user.target")

    # Phase 1: Compositor boots
    machine.wait_for_unit("ewwm-compositor.service", "testuser", timeout=30)
    machine.succeed(
        "test -S /run/user/$(id -u testuser)/ewwm-ipc.sock"
    )
    machine.succeed("pgrep -u testuser ewwm-compositor")

    # Phase 2: home-manager config.el generated with correct setq forms
    config_el = machine.succeed(
        "cat /home/testuser/.config/exwm-vr/config.el"
    )
    assert "setq ewwm-workspace-number 4" in config_el, (
        f"Missing workspace config in config.el: {config_el}"
    )
    assert "setq ewwm-floating-border-width 2" in config_el, (
        f"Missing border config in config.el: {config_el}"
    )
    assert "setq ewwm-vr-enable nil" in config_el, (
        f"Missing vr-enable config in config.el: {config_el}"
    )

    # Phase 3: Qutebrowser theme generated
    theme_py = machine.succeed(
        "cat /home/testuser/.config/qutebrowser/exwm-vr-theme.py"
    )
    assert "modus-vivendi" in theme_py, (
        f"Theme header missing: {theme_py}"
    )
    assert "c.colors.completion.fg" in theme_py, (
        f"Theme colors missing: {theme_py}"
    )

    # Phase 4: IPC hello handshake via protocol-correct helper
    result = machine.succeed(
        "python3 /etc/ipc-helper.py "
        "/run/user/$(id -u testuser)/ewwm-ipc.sock "
        "'(:type :command :command :version)'"
    )
    assert "version" in result.lower() or "response" in result.lower(), (
        f"Unexpected IPC response: {result}"
    )

    # Phase 5: Emacs daemon boots
    machine.wait_for_unit("ewwm-emacs.service", "testuser", timeout=60)

    # Phase 6: emacsclient eval — verify Emacs can load ewwm modules
    uid = machine.succeed("id -u testuser").strip()
    emacs_result = machine.succeed(
        f"su - testuser -c '"
        f"XDG_RUNTIME_DIR=/run/user/{uid} "
        "${pkgs.emacs-nox}/bin/emacsclient --eval "
        "\"(progn (require (quote ewwm-core)) (symbol-name (quote ewwm-core)))\"'"
    )
    assert "ewwm-core" in emacs_result, (
        f"Emacs failed to load ewwm-core: {emacs_result}"
    )

    # Phase 7: Clean shutdown
    machine.succeed(
        "systemctl --user -M testuser@ stop ewwm-emacs.service"
    )
    machine.succeed(
        "systemctl --user -M testuser@ stop ewwm-compositor.service"
    )
  '';
}
