{
  config,
  lib,
  ...
}: {
  imports = [
    ../global
    ../nixos
  ];

  home = {
    homeDirectory = lib.mkDefault "/home/${config.home.username}";

    packages = [];
  };
}
