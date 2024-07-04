{...}: {
  home.file.".spacemacs".source = ./spacemacs;
  home.file.".emacs.d/private/copilot-electric-indent-workaround.el".source = ./copilot-electric-indent-workaround.el;
  home.file.".emacs.d/private/emacs-lsp-booster.el".source = ./emacs-lsp-booster.el;
  home.file.".emacs.d/private/local/call-logging-hooks.el".source = ./local/call-logging-hooks.el;

  # ~/.emacs.d contains a clone of the spacemacs repo. Install via:
  #   git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
}
