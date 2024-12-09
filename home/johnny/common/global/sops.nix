{config, ...}: {
  sops = {
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
    defaultSopsFile = ./secrets.yaml;
    secrets = {
      "aws/config".path = "${config.home.homeDirectory}/.aws/config";
      "gradle.properties" = {
        path = "${config.xdg.configHome}/gradle/gradle.properties";
      };
      npmrc = {
        path = "${config.xdg.configHome}/npm/npmrc";
      };
    };
  };
}
