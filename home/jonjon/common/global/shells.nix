{
  config,
  pkgs,
  ...
}: let
  zshInitExtra = ''
    source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh

    function omz_history {
      local clear list
      zparseopts -E c=clear l=list

      if [[ -n "$clear" ]]; then
        # if -c provided, clobber the history file
        echo -n >| "$HISTFILE"
        fc -p "$HISTFILE"
        echo >&2 History file deleted.
      elif [[ -n "$list" ]]; then
        # if -l provided, run as if calling `fc' directly
        builtin fc "$@"
      else
        # unless a number is provided, show all history events (starting from 1)
        [[ ''${@[-1]-} = *[0-9]* ]] && builtin fc -l "$@" || builtin fc -l "$@" 1
      fi
    }

    export GPG_TTY=$(tty)

    export LSP_USE_PLISTS=true

    ${pkgs.fortune}/bin/fortune | ${pkgs.cowsay}/bin/cowsay -f llama
  '';
in {
  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
      historyControl = ["ignoredups" "ignorespace"];
    };
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };
    eza = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
    };
    fzf = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
    };
    starship = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };
    zsh = {
      autosuggestion.enable = true;
      enable = true;
      enableCompletion = true;
      dotDir = builtins.replaceStrings ["${config.home.homeDirectory}"] [""] "${config.xdg.configHome}/zsh";
      history = {
        path = "${config.xdg.dataHome}/zsh/zsh_history";
        expireDuplicatesFirst = true;
        extended = true;
        ignoreDups = true;
        ignoreSpace = true;
        share = true;
        save = 100000;
        size = 100000;
      };
      shellAliases = {
        # cat on steroids
        cat = "bat";

        history = "omz_history";
      };
      syntaxHighlighting.enable = true;
      initExtra = zshInitExtra;
    };
  };
}
