{
  inputs,
  self,
  ...
}: {
  home-manager = {
    # include flake's last modified timestamp to avoid conflicts
    backupFileExtension = "backup-${builtins.toString self.lastModified}";
    sharedModules = [
      inputs.sops-nix.homeManagerModules.sops
    ];
    useUserPackages = true;
    useGlobalPkgs = true;
  };
}
