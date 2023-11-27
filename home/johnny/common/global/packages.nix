{pkgs, ...}: {
  home.packages = with pkgs;
    [
      act
      age
      age-plugin-yubikey
      alejandra
      # use stable version due to error:
      # - error: assertion '((pyproject != null) -> (format == null))' failed
      # already fixed but not in `nixpkgs-unstable` yet: https://github.com/NixOS/nixpkgs/pull/268590
      stable.awscli2
      cargo
      cargo-udeps
      clipboard-jh
      curl
      dig
      docker
      docker-compose
      eslint_d # eslint server for emacs
      fd # faster find
      gdb
      gnupg
      go
      htop
      jetbrains-mono
      jq
      k6
      moreutils
      nil
      nodejs
      openssl
      packer
      python3
      ripgrep
      rover
      rustc
      rustfmt
      terraform
      unzip
      vips # for sharp
      wget
      yarn
      yq
      zip
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
