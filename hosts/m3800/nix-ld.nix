{pkgs, ...}: {
  # Support execution of unpackaged programs (e.g. rover plugins)
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    # Add any missing dynamic libraries for unpackaged
    # programs here, NOT in environment.systemPackages
  ];
}
