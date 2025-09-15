{
  description = "Johnny's Nix Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.05";
    darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";

    gitignore.url = "github:hercules-ci/gitignore.nix";
    gitignore.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    # emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.url = "github:johnnywalker/emacs-lsp-booster/dont-panic";
    # emacs-lsp-booster.url = "/home/johnny/source/johnny/emacs-lsp-booster";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs-unstable";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";

    # # Hyprspace does not yet build with 0.46.2
    # hyprland.url = "github:hyprwm/Hyprland/v0.45.2";
    # # compatible with 0.45.2
    # hyprspace.url = "github:KZDKM/Hyprspace/260f386075c7f6818033b05466a368d8821cde2d";
    # hyprspace.inputs.hyprland.follows = "hyprland";

    # hyprpolkitagent.url = "github:hyprwm/hyprpolkitagent/08cab3a4d9277687562702ae2db56305f9165081";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-darwin,
    nixpkgs-unstable,
    home-manager,
    darwin,
    gitignore,
    sops-nix,
    treefmt-nix,
    emacs-lsp-booster,
    emacs-overlay,
    # hyprland,
    # hyprspace,
    # hyprpolkitagent,
  }: let
    darwinSystems = ["x86_64-darwin" "aarch64-darwin"];
    supportedSystems = ["x86_64-linux" "aarch64-linux"] ++ darwinSystems;

    # Small tool to iterate over each systems
    eachSystem = f:
      nixpkgs.lib.genAttrs supportedSystems (system:
        if builtins.elem system darwinSystems
        # use darwin branch for macOS
        then f nixpkgs-darwin.legacyPackages.${system}
        else f nixpkgs.legacyPackages.${system});

    # Eval the treefmt modules from ./treefmt.nix
    treefmtEval = eachSystem (pkgs: treefmt-nix.lib.evalModule pkgs ./treefmt.nix);

    specialArgs = {inherit inputs self;};
  in {
    # for `nix fmt`
    formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);

    # for `nix flake check`
    checks = eachSystem (pkgs: {
      formatting = treefmtEval.${pkgs.system}.config.build.check self;
    });

    darwinConfigurations."des-jwmac" = darwin.lib.darwinSystem {
      inherit specialArgs;
      system = "x86_64-darwin";
      modules = [
        home-manager.darwinModules.home-manager
        self.darwinModules.home-manager-special-args
        self.darwinModules.nixpkgs
        self.darwinModules.des-jwmac
        {
          home-manager.users.johnny.imports = [self.homeModules."johnny/des-jwmac"];
        }
      ];
    };

    darwinModules = {
      home-manager-special-args = self.nixosModules.home-manager-special-args;
      nixpkgs = self.nixosModules.nixpkgs;
      des-jwmac = import ./hosts/des-jwmac;
    };

    homeModules = {
      "johnny/des-jwmac" = import ./home/johnny/des-jwmac;
      "johnny/iota" = import ./home/johnny/iota;
      "johnny/m3800" = import ./home/johnny/m3800;
      "johnny/petey" = import ./home/johnny/petey;
      "jonjon/iota" = import ./home/jonjon/iota;
    };

    nixosModules = {
      home-manager-special-args = {
        home-manager.extraSpecialArgs = specialArgs;
      };
      nixpkgs = {pkgs, ...}: {
        nixpkgs = {
          overlays = [
            # Any additional overlays can be placed here
            emacs-overlay.overlays.default
            (import ./elisp-packages)
            (final: prev: {
              unstable = import inputs.nixpkgs-unstable {
                inherit (final) config system;
              };

              # master = import inputs.nixpkgs-master {
              #   inherit (final) config system;
              # };

              # concat emacs config to scan for `use-package` calls
              emacs-config-concat = let
                inherit (gitignore.lib) gitignoreSource;
              in
                final.stdenv.mkDerivation {
                  name = "combined-config";
                  version = "0.1.0";
                  src = gitignoreSource ./emacs.d;
                  phases = ["unpackPhase" "installPhase"];
                  installPhase = ''
                    cat init.el > $out
                    cat lisp/*.el >> $out
                  '';
                };

              # emacs-lsp-booster = emacs-lsp-booster.packages.${pkgs.system}.default;
              llama-cpp = prev.llama-cpp.overrideAttrs (attrs: {
                version = "4545";
                src = final.fetchFromGitHub {
                  owner = "ggerganov";
                  repo = "llama.cpp";
                  tag = "b4545";
                  hash = "sha256-NUt6Q372jsCzcSAEqe2VZB2ZUpGSZyrvr0wyqrBYoOY=";
                  leaveDotGit = true;
                  postFetch = ''
                    git -C "$out" rev-parse --short HEAD > $out/COMMIT
                    find "$out" -name .git -print0 | xargs -0 rm -rf
                  '';
                };
                # doesn't seem to work
                HSA_OVERRIDE_GFX_VERSION = "11.0.0";
              });
            })
          ];
          config = {
            allowUnfree = true;
          };
        };
      };
      iota = import ./hosts/iota;
      m3800 = import ./hosts/m3800;
      petey = import ./hosts/petey;
    };

    nixosConfigurations = {
      iota = nixpkgs.lib.nixosSystem {
        inherit specialArgs;
        modules = [
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          self.nixosModules.home-manager-special-args
          self.nixosModules.nixpkgs
          self.nixosModules.iota
          {
            home-manager.users.johnny.imports = [self.homeModules."johnny/iota"];
            home-manager.users.jonjon.imports = [self.homeModules."jonjon/iota"];
          }
        ];
      };
      m3800 = nixpkgs.lib.nixosSystem {
        inherit specialArgs;
        modules = [
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          self.nixosModules.home-manager-special-args
          self.nixosModules.nixpkgs
          self.nixosModules.m3800
          {
            home-manager.users.johnny.imports = [self.homeModules."johnny/m3800"];
          }
        ];
      };
      petey = nixpkgs.lib.nixosSystem {
        inherit specialArgs;
        modules = [
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          self.nixosModules.home-manager-special-args
          self.nixosModules.nixpkgs
          self.nixosModules.petey
          {
            home-manager.users.johnny.imports = [self.homeModules."johnny/petey"];
          }
        ];
      };
    };
  };
}
