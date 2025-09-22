{pkgs, ...}: {
  imports = [
    ../common/presets/nixos.nix
    ./emacs
    ./hyprland.nix
    ./hyprpolkitagent.nix
    ./sway.nix
    ./waybar.nix
    ./wayland.nix
  ];

  home.packages = with pkgs;
    [
      gpt4all
      imagemagick
      inkscape
      jellyfin-media-player
      libreoffice
      mpv
      signal-desktop
      telegram-desktop
    ]
    ++ (
      # match mesa version if using hyprland flake
      # with pkgs-unstable;
      with pkgs; [
        loupe # image viewer
        oculante # image viewer
        swappy # simple image editor (for editing screenshots)
        vlc
      ]
    );
}
