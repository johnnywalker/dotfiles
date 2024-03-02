{pkgs, ...}: {
  nix = {
    distributedBuilds = true;

    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
    };

    # had trouble getting this to build the first time and had to bootstrap
    # https://nixos.org/manual/nixpkgs/unstable/#sec-darwin-builder
    # run builder manually: `nix run nixpkgs#darwin.linux-builder`
    linux-builder = {
      enable = true;
      maxJobs = 4;
      config = {
        virtualisation.cores = 4;
      };
    };

    package = pkgs.nix;

    settings = {
      # https://github.com/NixOS/nix/issues/7273
      auto-optimise-store = pkgs.stdenv.isLinux;
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

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  homebrew = {
    enable = true;
    brews = [
      "asimov" # automatically exclude dev dependencies from Time Machine
    ];
    casks = [
      "chromium"
      "font-fira-code-nerd-font"
      "font-jetbrains-mono-nerd-font"
      "librecad"
      "meld" # visual diff and merge tool
      "swiftbar" # custom menu bar apps on macOS
    ];
    # casks = [ "iterm2" "qtpass" ];
    # masApps = {
    #   Tailscale = 1475387142;
    # };
    onActivation = {
      autoUpdate = true;
      # uncomment to remove packages not listed above
      # cleanup = "uninstall";
    };
  };

  environment.shells = with pkgs; [
    bashInteractive
    # fish
    zsh
  ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    alacritty
    colima
    emacs29-macport
    # (import ./emacs.nix {inherit pkgs;})
    iconv # not sure why
    lima
    # neovide # build fails
    # nixos-generators
    # php-with-extensions # remove?
    qemu
  ];

  security.pam.enableSudoTouchIdAuth = true;

  system.defaults = {
    dock = {
      autohide = true;
      magnification = false;
      mru-spaces = false;
      orientation = "bottom";
      tilesize = 42;
      wvous-bl-corner = 1;
      wvous-br-corner = 1;
      wvous-tl-corner = 1;
      wvous-tr-corner = 1;
    };
    finder = {
      ShowPathbar = true;
      ShowStatusBar = true;
    };
    menuExtraClock.Show24Hour = true;
    spaces.spans-displays = false;
    trackpad.Clicking = true;
    NSGlobalDomain = {
      # Automatic dark mode at night
      AppleInterfaceStyleSwitchesAutomatically = true;

      AppleShowAllExtensions = true;

      # 15 milliseconds until the key repeats, then 2 milliseconds
      # between subsequent inputs. This can be achieved in the settings UI
      InitialKeyRepeat = 35;
      KeyRepeat = 2;
    };
  };

  users.users."johnny" = {
    shell = pkgs.zsh;
  };
}
