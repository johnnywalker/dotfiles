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
      emacs-lsp-booster
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
      kubectl
      # language server for scala
      # - use snapshot version with fix for https://github.com/scalameta/metals/issues/6472
      (metals.overrideAttrs rec {
        pname = "metals";
        version = "1.3.1+86-b8d71993-SNAPSHOT";
        deps = stdenv.mkDerivation {
          name = "${pname}-deps-${version}";
          buildCommand = ''
            export COURSIER_CACHE=$(pwd)
            ${coursier}/bin/cs fetch org.scalameta:metals_2.13:${version} \
              -r bintray:scalacenter/releases \
              -r sonatype:snapshots > deps
            mkdir -p $out/share/java
            cp $(< deps) $out/share/java/
          '';
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = "sha256-m/07rC3s7j6GPFOmA8VazNAVCxu9452Jit0nbtgwaSE=";
        };
        buildInputs = [deps];
      })
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
      (texlive.combine {
        inherit
          (texlive)
          scheme-basic
          amsmath
          booktabs
          capt-of
          collection-metapost
          etoolbox
          hyperref
          mdwtools
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
