{
  description = "EXWM-VR: VR-first transhuman Emacs window manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, emacs-overlay, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            emacs-overlay.overlays.default
            rust-overlay.overlays.default
          ];
        };

        rustToolchain = pkgs.rust-bin.nightly.latest.default.override {
          extensions = [
            "rustc"
            "cargo"
            "clippy"
            "rustfmt"
            "rust-analyzer"
            "rust-src"
          ];
        };

        emacsPkg = pkgs.emacs-pgtk.overrideAttrs (old: {
          configureFlags = (old.configureFlags or [ ]) ++ [
            "--with-native-compilation=aot"
          ];
        });

        waylandLibs = with pkgs; [
          wayland
          wayland-protocols
          wayland-scanner
          libdrm
          mesa
          libinput
          libxkbcommon
          seatd
          libffi
          pixman
          udev
        ];

        buildInputs = with pkgs; [
          # Rust
          rustToolchain
          pkg-config
          clang
          llvmPackages.libclang

          # Emacs
          emacsPkg

          # Wayland / compositor deps
        ] ++ waylandLibs ++ [
          # OpenXR / VR
          monado
          openxr-loader

          # Dev tools
          just
          git-cliff
          nixpkgs-fmt
          direnv

          # Testing
          cage   # single-window Wayland compositor for testing
          weston # headless Wayland compositor
        ];

      in {
        devShells.default = pkgs.mkShell {
          inherit buildInputs;

          shellHook = ''
            export LIBCLANG_PATH="${pkgs.llvmPackages.libclang.lib}/lib"
            export PKG_CONFIG_PATH="${pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig" waylandLibs}"
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath waylandLibs}"
            export XDG_DATA_DIRS="$XDG_DATA_DIRS:${pkgs.monado}/share"
            export OPENXR_RUNTIME_JSON="${pkgs.monado}/share/openxr/1/openxr_monado.json"
            echo "exwm-vr dev shell ready"
            echo "  rustc: $(rustc --version)"
            echo "  emacs: $(emacs --version | head -1)"
          '';
        };

        # Placeholder for compositor package (Week 3)
        # packages.compositor = ...;

        # Placeholder for NixOS module (Week 15)
        # nixosModules.default = ...;
      }
    );
}
