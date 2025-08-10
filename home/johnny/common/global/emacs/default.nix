{
  config,
  lib,
  pkgs,
  ...
}: {
  config = {
    # TODO use replaceVars https://noogle.dev/f/pkgs/replaceVars
    home.file.".spacemacs".source = pkgs.substituteAll {
      src = ./spacemacs;
      dotspacemacs_font_size = builtins.toString config.programs.spacemacs.fontSize;
      dotspacemacs_enable_server =
        if config.programs.spacemacs.enableServer
        then "t"
        else "nil";
      dotspacemacs_persistent_server =
        if config.programs.spacemacs.persistentServer
        then "t"
        else "nil";
    };
    home.file.".emacs.d/private/copilot-electric-indent-workaround.el".source = ./copilot-electric-indent-workaround.el;
    home.file.".emacs.d/private/emacs-lsp-booster.el".source = ./emacs-lsp-booster.el;
    home.file.".emacs.d/private/nickel-mode.el".source = ./nickel-mode.el;
    home.file.".emacs.d/private/local/call-logging-hooks.el".source = ./local/call-logging-hooks.el;

    home.packages = with pkgs; [
      (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
      nil # language server for nix
      ruff # formatter, language server for python
      terraform-ls # language server for terraform
    ];
    # ~/.emacs.d contains a clone of the spacemacs repo. Install via:
    #   git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
  };

  options.programs.spacemacs = {
    fontSize = lib.mkOption {
      type = lib.types.numbers.positive;
      default = 13.0;
    };
    enableServer = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
    persistentServer = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };
}
