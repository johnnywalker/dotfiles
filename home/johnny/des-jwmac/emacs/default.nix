{config, ...}: {
  sops.secrets."emacs/init-host.el".path = "${config.xdg.configHome}/emacs/lisp/init-host.el";
}
