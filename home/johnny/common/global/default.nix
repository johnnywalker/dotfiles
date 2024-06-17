{
  config,
  lib,
  ...
}: {
  imports = [
    ./authinfo
    ./bat.nix
    ./emacs
    ./git.nix
    ./neovim.nix
    ./packages.nix
    ./shells.nix
    ./sops.nix
  ];

  home = {
    username = lib.mkDefault "johnny";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "22.11";

    sessionVariables = {
      LANG = "en_US.UTF-8";
      LC_CTYPE = "en_US.UTF-8";
      GRADLE_USER_HOME = "${config.xdg.configHome}/gradle";
      NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/npmrc";
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
