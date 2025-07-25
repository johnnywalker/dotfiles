{pkgs, ...}: {
  home.packages = with pkgs;
    [
      act
      actionlint # GitHub action linter
      age
      age-plugin-yubikey
      alejandra
      awscli2
      unstable.azure-cli
      bc
      bitwarden-cli
      cargo
      cargo-udeps
      clipboard-jh
      curl
      dig
      docker
      docker-buildx
      docker-compose
      # emacs-lsp-booster
      # wrap emacs-lsp-booster to set RUST_BACKTRACE=1
      (pkgs.runCommand "emacs-lsp-booster" {
          buildInputs = [makeWrapper];
        } ''
          mkdir -p $out/bin
          cp ${emacs-lsp-booster}/bin/* $out/bin/
          wrapProgram $out/bin/emacs-lsp-booster \
            --set RUST_BACKTRACE 1
        '')
      eslint_d # eslint server for emacs
      fd # faster find
      gdb
      gnumake
      gnupg
      gnutar
      go
      htop
      iperf # network performance testing
      jq
      k6
      kubectl
      kubernetes-helm
      libxml2 # for xmllint
      # language server for scala
      metals
      moreutils
      nil
      nodejs
      openjdk
      openssl
      packer
      pandoc
      pipenv
      pre-commit
      pstree
      python3
      ripgrep
      rover
      ruff # formatter, language server for python
      rustc
      rustfmt
      scc # code statistics
      sipcalc # subnet calculator
      sops
      ssm-session-manager-plugin
      terraform
      terraform-ls # language server for terraform
      (texlive.combine {
        inherit
          (texlive)
          scheme-basic
          amsmath
          booktabs
          capt-of
          collection-metapost
          etoolbox
          fancyvrb
          hyperref
          mdwtools
          multirow
          soul
          ulem
          wrapfig
          xcolor
          ;
      })
      unixtools.watch
      unzip
      vips # for sharp
      wget
      yarn
      yq
      zip
      zstd
    ]
    ++ (with nodePackages; [
      node-gyp
      npm-check-updates
      prettier
      typescript-language-server
    ])
    ++ (with pkgs.haskellPackages; [
      apply-refact
      # haskell-language-server
      hasktags
      hlint
      hoogle
      stylish-haskell
    ]);
}
