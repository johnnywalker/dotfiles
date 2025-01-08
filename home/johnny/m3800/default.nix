{pkgs, ...}: {
  imports = [
    ../common/presets/nixos.nix
    ./sway.nix
  ];

  programs.git = {
    signing = {
      key = "1981B439C9CDE11F4C76606759832CD4EA037F20";
      signByDefault = true;
    };
  };

  home.file.".authinfo.gpg".source = ./authinfo.gpg;
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
