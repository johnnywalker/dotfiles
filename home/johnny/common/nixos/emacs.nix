{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-pgtk;
    extraPackages = epkgs: (with epkgs; [
      bind-map
      evil
      evil-args
      evil-collection
      evil-exchange
      evil-matchit
      evil-nerd-commenter
      evil-surround
      # (treesit-grammars.with-grammars (grammars: with grammars; [tree-sitter-bash tree-sitter-dockerfile]))
      treesit-grammars.with-all-grammars
      tree-sitter-langs
      vterm
    ]);
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
  xdg.desktopEntries.emacs-vanilla = {
    name = "Emacs (Vanilla)";
    genericName = "Text Editor";
    exec = ''
      emacs --init-directory /home/johnny/.emacs.vanilla.d %F
    '';
    terminal = false;
    icon = "emacs";
    categories = ["Development" "TextEditor"];
    type = "Application";
    settings = {
      StartupNotify = "true";
      StartupWMClass = "Emacs";
    };
  };
}
