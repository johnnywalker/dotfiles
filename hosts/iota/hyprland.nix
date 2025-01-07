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
  programs.hyprland.withUWSM = true;
  # uncomment to use hyprland flake
  # programs.hyprland.package = hyprland-pkgs.hyprland;
  # programs.hyprland.portalPackage = hyprland-pkgs.xdg-desktop-portal-hyprland;

  environment.systemPackages = with pkgs; [
    # fix mesa version mismatch if using hyprland flake
    # pkgs-unstable.kitty
    kitty
  ];

  # match mesa version if using hyprland flake
  # hardware.graphics.package = pkgs-unstable.mesa.drivers;
}
