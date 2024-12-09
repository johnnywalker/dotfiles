{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../global
    ../nix-darwin
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
