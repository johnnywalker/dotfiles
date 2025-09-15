{
  inputs,
  pkgs,
  ...
}: let
  pkgs-unstable = inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in {
  imports = [
    ../common/presets/nixos.nix
    ./emacs
    ./hyprland.nix
    ./hyprpolkitagent.nix
    ./jetbrains.nix
    ./mpd.nix
    ./pass.nix
    ./swaync.nix
    ./teams-for-linux.nix
    ./waybar.nix
  ];

  home.packages = with pkgs;
    [
      cameractrls-gtk4
      font-manager
      ghostty
      gimp
      libreoffice
      mattermost-desktop
      obs-studio
      qalculate-gtk
      signal-desktop
      slack
      sql-formatter
      teams-for-linux
      pkgs.unstable.zed-editor.fhs
      # zed-editor.fhs
      zulip
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

  services.blueman-applet.enable = true;
  services.trayscale.enable = true;
}
