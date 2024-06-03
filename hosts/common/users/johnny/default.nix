{
  config,
  inputs,
  pkgs,
  ...
}: {
  home-manager = {
    users = {
      johnny = import "${inputs.self}/home/johnny/${config.networking.hostName}.nix";
    };
  };

  users = {
    mutableUsers = false;
    users = {
      johnny = {
        description = "Johnny Walker";
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets.johnny-password.path;
        shell = pkgs.zsh;
        extraGroups = ["networkmanager" "wheel" "docker"];
      };
    };
  };

  sops = {
    secrets = {
      johnny-password = {
        sopsFile = ./secrets.yaml;
        neededForUsers = true;
      };
    };
  };
}
