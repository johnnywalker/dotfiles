{pkgs, ...}: {
  home.packages = with pkgs;
    [
      act
      age
      age-plugin-yubikey
      alejandra
      awscli
      cargo
      cargo-udeps
      clipboard-jh
      curl
      dig
      docker
      docker-compose
      gdb
      gnupg
      go
      htop
      jetbrains-mono
      jq
      moreutils
      nil
      nodejs
      openssl
      python3Minimal
      ripgrep
      rover
      rustc
      rustfmt
      unzip
      wget
      yarn
      yq
      zip
    ]
    ++ (with nodePackages; [
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
