{
  inputs,
  self,
  ...
}: {
  home-manager = {
    # include flake's last modified timestamp to avoid conflicts
    backupFileExtension = "backup-$(date +%Y%m%d)";
    sharedModules = [
      inputs.sops-nix.homeManagerModules.sops
    ];
    useUserPackages = true;
    useGlobalPkgs = true;
  };
}
