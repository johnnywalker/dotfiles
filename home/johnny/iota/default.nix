{
  inputs,
  pkgs,
  ...
}: let
  pkgs-unstable = inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in {
  imports = [
    ../common/presets/nixos.nix
    ./hyprland.nix
    ./hyprpolkitagent.nix
    ./jetbrains.nix
    ./mpd.nix
    ./teams-for-linux.nix
    ./waybar.nix
  ];

  home.packages = with pkgs;
    [
      cameractrls-gtk4
      font-manager
      jetbrains.datagrip
      libreoffice
      obs-studio
      qalculate-gtk
      slack
      teams-for-linux
    ]
    ++ (
      # match mesa version if using hyprland flake
      # with pkgs-unstable;
      with pkgs; [
        loupe # image viewer
        oculante # image viewer
        photoqt # image viewer
        satty
        swappy # simple image editor (for editing screenshots)
        vimiv-qt # image viewer
        vlc
      ]
    );
}
