{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-pgtk;
    extraPackages = epkgs: [
      # (epkgs.treesit-grammars.with-grammars (grammars: with grammars; [tree-sitter-bash tree-sitter-dockerfile]))
      epkgs.treesit-grammars.with-all-grammars
      epkgs.tree-sitter-langs
      epkgs.vterm
    ];
  };
  programs.spacemacs = {
    fontSize = 10.5;
    enableServer = true;
    persistentServer = true;
  };
  services.emacs.client.enable = true;
  # TODO try re-enabling service again - ran into issues previously with client hanging
  # services.emacs.enable = true;
  # services.emacs.socketActivation.enable = true;
  # services.emacs.startWithUserSession = true;
}
