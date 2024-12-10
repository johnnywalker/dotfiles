{
  inputs,
  pkgs,
  ...
}: let
  hyprland-pkgs = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system};
  pkgs-unstable = inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in {
  nix.settings = {
    substituters = ["https://hyprland.cachix.org"];
    trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
  };
  # Use SDDM with Hyprland
  services.displayManager.sddm.enable = true;
  programs.hyprland.enable = true;
  # programs.hyprland.package = pkgs.unstable.hyprland;
  programs.hyprland.package = hyprland-pkgs.hyprland;
  # programs.hyprland.portalPackage = pkgs.unstable.xdg-desktop-portal-hyprland;
  programs.hyprland.portalPackage = hyprland-pkgs.xdg-desktop-portal-hyprland;

  environment.systemPackages = with pkgs; [
    # use unstable due to mesa version mismatch
    unstable.kitty
  ];

  # match mesa version to avoid hyprland crash
  hardware.graphics.package = pkgs-unstable.mesa.drivers;
}
