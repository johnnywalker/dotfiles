{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../global
  ];

  home = {
    homeDirectory = lib.mkDefault "/Users/${config.home.username}";

    packages = with pkgs; [
      # not found
      # pinentry-mac
      wireguard-go
      wireguard-tools
    ];
  };
}
