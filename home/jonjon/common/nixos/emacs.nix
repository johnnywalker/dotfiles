{pkgs, ...}: {
  home.packages = [
    # build emacs packages from `use-package` macros in emacs config
    (pkgs.emacsWithPackagesFromUsePackage {
      config = pkgs.emacs-config-concat;
      package = pkgs.emacs30-pgtk;
      extraEmacsPackages = epkgs: (with epkgs; [
        treesit-grammars.with-all-grammars
        tree-sitter-langs
      ]);
      override = import ../../../../elisp-packages;
    })
  ];
  services.emacs.client.enable = true;
  # TODO try re-enabling service again - ran into issues previously with client hanging
  # services.emacs.enable = true;
  # services.emacs.socketActivation.enable = true;
  # services.emacs.startWithUserSession = true;
}
