{
  config,
  pkgs,
  ...
}: {
  users = {
    mutableUsers = false;
    users = {
      henry = {
        description = "Henry Walker";
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets.henry-password.path;
        shell = pkgs.zsh;
      };
    };
  };

  sops = {
    secrets = {
      henry-password = {
        sopsFile = ./secrets.yaml;
        neededForUsers = true;
      };
    };
  };
}
