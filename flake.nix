{
  description = "Johnny's Nix Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-24.05-darwin";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    emacs-lsp-booster.url = "github:slotThe/emacs-lsp-booster-flake";
    emacs-lsp-booster.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-darwin,
    nixpkgs-unstable,
    home-manager,
    darwin,
    sops-nix,
    treefmt-nix,
    emacs-lsp-booster,
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

    specialArgs = {inherit inputs;};

    overlays = [
      (prev: final: {
        unstable = import nixpkgs-unstable {inherit (prev) system;};
      })
      emacs-lsp-booster.overlays.default
    ];
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
        ./hosts/des-jwmac/default.nix
        home-manager.darwinModules.home-manager
        {
          home-manager = {
            sharedModules = [
              sops-nix.homeManagerModules.sops
            ];
            useGlobalPkgs = true;
            useUserPackages = true;
            users.johnny = import ./home/johnny/des-jwmac;
          };
          nixpkgs = {
            inherit overlays;
            # for terraform
            config.allowUnfree = true;
          };
          # https://github.com/LnL7/nix-darwin/issues/682
          users.users.johnny.home = "/Users/johnny";
        }
      ];
    };

    nixosConfigurations = {
      m3800 = nixpkgs.lib.nixosSystem {
        inherit specialArgs;
        modules = [
          "${self}/hosts/m3800"
          {
            home-manager = {
              users.johnny = import ./home/johnny/m3800.nix;
            };
          }
        ];
      };
    };
  };
}
