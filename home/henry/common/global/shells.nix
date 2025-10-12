{config, ...}: {
  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
      historyControl = ["ignoredups" "ignorespace"];
    };
    eza = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
    };
    fzf = {
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
      syntaxHighlighting.enable = true;
    };
  };
}
