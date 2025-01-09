{pkgs, ...}: {
  imports = [
    ../common/presets/nixos.nix
    ./sway.nix
  ];

  home.packages = with pkgs; [
    gpt4all
    imagemagick
    inkscape
    libreoffice
    mpv
    signal-desktop
    telegram-desktop
    vlc
  ];
}
