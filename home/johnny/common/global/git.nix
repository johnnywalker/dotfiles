{lib, ...}: {
  programs = {
    gh = {
      enable = true;
    };
    git = {
      enable = true;
      userName = "Johnny Walker";
      userEmail = lib.mkDefault "748017+johnnywalker@users.noreply.github.com";

      delta = {
        enable = true;
        options = {
          dark = true;
          features = "side-by-side";
          hyperlinks = false;
          line-numbers = true;
          navigate = true;
          syntax-theme = "Dracula";
        };
      };

      extraConfig = {
        color = {
          ui = true;
        };

        core = {
          editor = "nvim -u NONE";
          filemode = false;
          ignorecase = false;
        };

        github = {
          user = "johnnywalker";
        };

        init = {
          defaultBranch = "main";
        };

        merge = {
          # Include common parent when merge conflicts arise
          conflictStyle = "diff3";
        };

        pull = {
          ff = "only";
        };
      };
    };
  };
}
