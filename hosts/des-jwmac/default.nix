{pkgs, ...}: {
  nix = {
    distributedBuilds = true;

    # gc = {
    #   automatic = true;
    #   dates = "daily";
    #   options = "--delete-older-than 14d";
    #   persistent = true;
    # };

    # had trouble getting this to build the first time and had to bootstrap
    # https://nixos.org/manual/nixpkgs/unstable/#sec-darwin-builder
    # run builder manually: `nix run nixpkgs#darwin.linux-builder`
    linux-builder = {
      enable = true;
      maxJobs = 4;
      modules = [
        {virtualisation.cores = 4;}
        # ({config, ...}: {
        #   virtualisation.cores = 4;
        # })
      ];
    };

    # optimise = {
    #   automatic = true;
    #   dates = ["daily"];
    # };

    package = pkgs.nix;

    settings = {
      # auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes" "repl-flake"];
      # keep-derivations = true;
      # keep-outputs = true;
      trusted-users = ["root" "@admin"];
      # min-free = lib.mkDefault (10 * 1000 * 1000 * 1000); # 10gb
      # cores = 0;
      max-jobs = 2;

      # builders = "ssh-ng://builder@linux-builder x86_64-linux /etc/nix/builder_ed25519 4 - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=";
    };
  };

  # Make sure the nix daemon always runs
  services.nix-daemon.enable = true;
  # Install a version of nix, that dosen't need "experimental-features = nix-command flakes" in /etc/nix/nix.conf
  # services.nix-daemon.package = pkgs.nixFlakes;

  # enable this so nix-darwin creates a zshrc sourcing needed environment changes
  programs.zsh.enable = true;
  # bash is enabled by default

  # homebrew = {
  #   enable = true;
  #   autoUpdate = true;

  #   casks = [];
  # };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    act
    alejandra
    cargo
    cargo-udeps
    colima
    curl
    docker-compose
    direnv
    emacs29-macport
    # (import ./emacs.nix {inherit pkgs;})
    evcxr
    git
    gnupg
    iconv # not sure why
    jq
    htop
    # neovide # build fails
    neovim
    nil
    # nixos-generators
    # php-with-extensions # remove?
    ripgrep
    rustc
    rustfmt
    vim
    wireguard-go
    wireguard-tools
  ];

  security.pam.enableSudoTouchIdAuth = true;

  system.defaults.dock.autohide = true;
}
