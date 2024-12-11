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
    ./mpd.nix
    ./teams-for-linux.nix
  ];

  home.packages = with pkgs; [
    libreoffice
    pkgs-unstable.loupe # image viewer
    pkgs-unstable.oculante # image viewer
    pkgs-unstable.photoqt # image viewer
    pkgs-unstable.obs-studio # use hyprland nixpkgs to match mesa version
    teams-for-linux
    pkgs-unstable.satty
    pkgs-unstable.swappy
    pkgs-unstable.vimiv-qt # image viewer
    pkgs-unstable.vlc
  ];

  programs.git = {
    userEmail = "jwalker@designsforhealth.com";
    signing = {
      key = "637096B053DC9185AA43EB7CE066C68A21EFECDE";
      signByDefault = true;
    };
  };

  home.file.".authinfo.gpg".source = ./authinfo.gpg;
}
