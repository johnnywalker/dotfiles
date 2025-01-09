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

  home.packages = with pkgs; [
    cameractrls-gtk4
    font-manager
    jetbrains.datagrip
    libreoffice
    # match mesa version if using hyprland flake
    # pkgs-unstable.loupe # image viewer
    # pkgs-unstable.oculante # image viewer
    # pkgs-unstable.photoqt # image viewer
    # pkgs-unstable.obs-studio
    loupe # image viewer
    oculante # image viewer
    photoqt # image viewer
    obs-studio
    qalculate-gtk
    slack
    teams-for-linux
    # match mesa version if using hyprland flake
    # pkgs-unstable.satty
    # pkgs-unstable.swappy
    # pkgs-unstable.vimiv-qt # image viewer
    # pkgs-unstable.vlc
    satty
    swappy
    vimiv-qt # image viewer
    vlc
  ];
}
