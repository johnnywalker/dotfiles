{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-macport;
    extraPackages = epkgs: [
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
}
