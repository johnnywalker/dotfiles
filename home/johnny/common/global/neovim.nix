{pkgs, ...}: {
  home = {
    sessionVariables = {
      # no profile makes it start faster than the speed of light
      EDITOR = "nvim -u NONE";
      VISUAL = "nvim -u NONE";
      GIT_EDITOR = "nvim -u NONE";
    };
  };

  programs = {
    neovim = {
      enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      withPython3 = false;
      withNodeJs = false;
      withRuby = false;
      extraPackages = [
        # this won't be useful globally, so neovim only is fine
        pkgs.shellcheck
      ];
      plugins = with pkgs.vimPlugins; [
        # dependencies
        plenary-nvim
        nui-nvim
        nvim-web-devicons
        # treesitter
        nvim-treesitter.withAllGrammars
        nvim-treesitter-context
        nvim-treesitter-textobjects
        # completion
        {
          plugin = nvim-cmp;
          optional = false;
        }
        {
          plugin = cmp-buffer;
          optional = false;
        }
        {
          plugin = cmp-nvim-lsp;
          optional = false;
        }
        {
          plugin = cmp-path;
          optional = false;
        }
        {
          plugin = cmp_luasnip;
          optional = false;
        }
        # lsp
        {
          plugin = nvim-lspconfig;
          optional = false;
        }
        {
          plugin = neodev-nvim;
          optional = false;
        }
        {
          plugin = null-ls-nvim;
          optional = false;
        }
        {
          plugin = luasnip;
          optional = false;
        }
        {
          plugin = friendly-snippets;
          optional = false;
        }
        {
          plugin = fidget-nvim;
          optional = false;
        }
        # dap
        {
          plugin = nvim-dap;
          optional = true;
        }
        {
          plugin = nvim-dap-ui;
          optional = true;
        }
        {
          plugin = nvim-dap-virtual-text;
          optional = true;
        }
        # telescope
        {
          plugin = telescope-nvim;
          optional = false;
        }
        {
          plugin = telescope-fzf-native-nvim;
          optional = false;
        }
        # statusline
        lualine-nvim
        nvim-navic
        # misc
        boole-nvim
        comment-nvim
        {
          plugin = diffview-nvim;
          optional = false;
        }
        gitsigns-nvim
        {
          plugin = harpoon;
          optional = false;
        }
        impatient-nvim
        indent-blankline-nvim
        vim-sleuth
        which-key-nvim
        # ui
        {
          plugin = catppuccin-nvim;
          optional = false;
        }
        dressing-nvim
        {
          plugin = neo-tree-nvim;
          optional = false;
        }
      ];
    };
  };
}
