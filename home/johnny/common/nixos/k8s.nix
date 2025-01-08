{config, ...}: {
  home.sessionVariables = {
    KUBECACHEDIR = "${config.xdg.cacheHome}/kube";
    KUBECONFIG = "${config.xdg.configHome}/kube/config";
  };
}
