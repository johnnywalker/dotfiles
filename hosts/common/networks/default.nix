{...}: {
  sops = {
    secrets = {
      wireless-secrets = {
        sopsFile = ./secrets.yaml;
      };
    };
  };
}
