{
  description = "Johnny's Nix Configuration";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin"; # newest version as of may 2023, probably needs to get updated in november
    # use unstable for linux-builder
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    darwin,
  }: {
    darwinConfigurations."des-jwmac" = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [
        # home-manager.darwinModules.home-manager
        ./hosts/des-jwmac/default.nix
      ];
    };
  };
}
