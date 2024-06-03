{...}: {
  programs = {
    gh = {
      enable = true;
    };
    git = {
      enable = true;
      userName = "Johnny Walker";
      userEmail = "748017+johnnywalker@users.noreply.github.com";

      signing = {
        key = "637096B053DC9185AA43EB7CE066C68A21EFECDE";
        signByDefault = true;
      };

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
