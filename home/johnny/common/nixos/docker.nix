{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    amazon-ecr-credential-helper
  ];

  home.sessionVariables = {
    DOCKER_CONFIG = "${config.xdg.configHome}/docker";
  };
}
