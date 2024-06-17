{pkgs, ...}: {
  home.packages = with pkgs;
    [
      act
      actionlint # GitHub action linter
      age
      age-plugin-yubikey
      alejandra
      awscli2
      bitwarden-cli
      cargo
      cargo-udeps
      clipboard-jh
      curl
      dig
      docker
      docker-buildx
      docker-compose
      eslint_d # eslint server for emacs
      fd # faster find
      gdb
      gnumake
      gnupg
      gnutar
      go
      htop
      iperf # network performance testing
      ispell # spell check for emacs
      # jetbrains-mono
      jq
      k6
      metals # language server for scala
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
      ruff
      ruff-lsp # language server for python
      rustc
      rustfmt
      scc # code statistics
      sipcalc # subnet calculator
      sops
      terraform
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
