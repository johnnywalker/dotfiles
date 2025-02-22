{
  inputs,
  pkgs,
  ...
}: let
  pkgs-unstable = inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in {
  imports = [
    ../common/nixos
    ./hyprland.nix
    ./hyprpolkitagent.nix
    ./mpd.nix
    ./pass.nix
    ./waybar.nix
  ];

  home.packages = with pkgs;
    [
      cameractrls-gtk4
      element-desktop
      font-manager
      ghostty
      gimp
      libreoffice
      obs-studio
      qalculate-gtk
      signal-desktop
      sql-formatter
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
