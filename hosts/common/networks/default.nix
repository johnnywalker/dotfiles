{...}: {
  sops = {
    secrets = {
      wireless-env = {
        sopsFile = ./secrets.yaml;
      };
    };
  };
}
