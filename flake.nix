{
  description = "Johnny's Nix Configuration";

  inputs = {
    # use unstable for linux-builder - consider replacing with nixpkgs-23.11-darwin once released in Nov 2023
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    darwin,
    treefmt-nix,
  }: let
    supportedSystems = ["x86_64-darwin" "aarch64-darwin"];

    # Small tool to iterate over each systems
    eachSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f nixpkgs.legacyPackages.${system});

    # Eval the treefmt modules from ./treefmt.nix
    treefmtEval = eachSystem (pkgs: treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
  in {
    # for `nix fmt`
    formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);

    # for `nix flake check`
    checks = eachSystem (pkgs: {
      formatting = treefmtEval.${pkgs.system}.config.build.check self;
    });

    darwinConfigurations."des-jwmac" = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [
        ./hosts/des-jwmac/default.nix
        home-manager.darwinModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.johnny = import ./home/johnny/des-jwmac.nix;
          };
          # for terraform
          nixpkgs.config.allowUnfree = true;
          # https://github.com/LnL7/nix-darwin/issues/682
          users.users.johnny.home = "/Users/johnny";
        }
      ];
    };
  };
}
