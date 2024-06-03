{
  config,
  lib,
  ...
}: {
  imports = [
    ../global
  ];

  home = {
    homeDirectory = lib.mkDefault "/home/${config.home.username}";

    packages = [];
  };
}
