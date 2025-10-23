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
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK52M5lAGnnRDpjYnPPgZX9Lz5SEfvARj23ecUPSvBHX"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHhL2s7nRiFBw8U0SMQPWCsaWQXc51YMP8ga81Uqm9Rx"
          # johnny@petey
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHudwtU+wqmxkbhlsg8/Lv8FII0Vu68xf9xHVxojkStM"
        ];
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
