{
  config,
  lib,
  ...
}: {
  imports = [
    ./packages.nix
    ./shells.nix
  ];

  home = {
    username = lib.mkDefault "henry";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "22.11";

    sessionVariables = {
      LANG = "en_US.UTF-8";
      LC_CTYPE = "en_US.UTF-8";
    };
  };

  programs = {
    home-manager = {
      enable = true;
    };
  };

  xdg = {
    enable = true;
    configHome = "${config.home.homeDirectory}/.config";
    cacheHome = "${config.home.homeDirectory}/.cache";
    dataHome = "${config.home.homeDirectory}/.local/share";
    stateHome = "${config.home.homeDirectory}/.local/state";
  };
}
