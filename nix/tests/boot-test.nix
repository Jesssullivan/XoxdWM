## NixOS integration test: validates compositor -> IPC boot chain.
##
## Run via: nix build .#checks.x86_64-linux.boot-test
## Requires KVM (QEMU VM-based test).

{ pkgs, self, ... }:

let
  ipcHelper = pkgs.writeScript "ipc-helper" (builtins.readFile ./ipc-helper.py);
in
pkgs.testers.nixosTest {
  name = "exwm-vr-boot-test";

  nodes.machine = { config, pkgs, ... }: {
    imports = [
      self.nixosModules.exwm-vr
    ];

    # Use the headless compositor (no GPU required in VM)
    services.exwm-vr = {
      enable = true;
      compositor.package = self.packages.${pkgs.system}.compositor-headless;
      compositor.extraArgs = [ "--backend" "headless" ];
    };

    # Create a test user in the ewwm group
    users.users.testuser = {
      isNormalUser = true;
      extraGroups = [ "ewwm" "video" "input" ];
    };

    # Override compositor service for test (headless VM has no graphical-session)
    systemd.user.services.ewwm-compositor = {
      wantedBy = pkgs.lib.mkForce [ "default.target" ];
      serviceConfig = {
        ExecStart = pkgs.lib.mkForce
          "${self.packages.${pkgs.system}.compositor-headless}/bin/ewwm-compositor --backend headless";
        Restart = pkgs.lib.mkForce "no";
        ProtectHome = pkgs.lib.mkForce false;
        SupplementaryGroups = pkgs.lib.mkForce [ ];
      };
    };

    # Disable Emacs service for boot-chain test (just test compositor)
    systemd.user.services.ewwm-emacs.enable = false;

    # Enable lingering so user services start without login
    users.users.testuser.linger = true;

    # IPC helper available in the VM
    environment.etc."ipc-helper.py".source = ipcHelper;

    # Python for IPC helper
    environment.systemPackages = [ pkgs.python3Minimal ];

    # Minimal VM configuration
    virtualisation.memorySize = 1024;
  };

  testScript = ''
    import time

    machine.start()
    machine.wait_for_unit("multi-user.target")

    # Give user services time to start
    time.sleep(3)

    # Debug: show compositor service status before asserting
    status = machine.execute(
        "systemctl --user -M testuser@ status ewwm-compositor.service 2>&1 || true"
    )
    machine.log(f"Compositor status: {status}")
    journal = machine.execute(
        "journalctl --user -M testuser@ -u ewwm-compositor.service --no-pager -n 30 2>&1 || true"
    )
    machine.log(f"Compositor journal: {journal}")

    # Wait for the compositor service to start
    machine.wait_for_unit("ewwm-compositor.service", "testuser", timeout=30)

    # Verify IPC socket exists
    machine.succeed(
        "test -S /run/user/$(id -u testuser)/ewwm-ipc.sock"
    )

    # Send a version query via length-prefixed IPC protocol
    result = machine.succeed(
        "python3 /etc/ipc-helper.py "
        "/run/user/$(id -u testuser)/ewwm-ipc.sock "
        "'(:type :command :command :version)'"
    )
    assert "version" in result.lower() or "response" in result.lower(), (
        f"Unexpected IPC response: {result}"
    )

    # Verify compositor process is running
    machine.succeed("pgrep -u testuser ewwm-compositor")

    # Clean shutdown
    machine.succeed(
        "systemctl --user -M testuser@ stop ewwm-compositor.service"
    )
  '';
}
