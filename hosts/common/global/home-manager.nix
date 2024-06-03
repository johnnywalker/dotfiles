{inputs, ...}: {
  home-manager = {
    sharedModules = [
      inputs.sops-nix.homeManagerModules.sops
    ];
    useUserPackages = true;
    useGlobalPkgs = true;
  };
}
