{
  # inputs,
  pkgs,
  ...
}: {
  imports = [
    ../common/global/home-manager.nix
    ../common/global/nix/darwin.nix
    ./trampolines.nix
  ];

  nix = {
    distributedBuilds = true;

    extraOptions = ''
      plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins
      # keep-outputs = true
      # keep-derivations = true
    '';

    # Decrypt at eval time - useful for NIX_NPM_TOKENS
    # ref: https://elvishjerricco.github.io/2018/06/24/secure-declarative-key-management.html
    envVars = builtins.fromJSON (builtins.extraBuiltins.decrypt "hosts/des-jwmac/secrets/nix-env-vars.json");

    # had trouble getting this to build the first time and had to bootstrap
    # https://nixos.org/manual/nixpkgs/unstable/#sec-darwin-builder
    # run builder manually: `nix run nixpkgs#darwin.linux-builder`
    linux-builder = {
      enable = false;
      maxJobs = 4;
      config = {
        virtualisation.cores = 4;
      };
    };

    settings = {
      # cores = 0;
      max-jobs = 2;

      # builders = "ssh-ng://builder@linux-builder x86_64-linux /etc/nix/builder_ed25519 4 - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=";
    };
  };

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
    colima
    grandperspective # visualize disk usage
    # (import ./emacs.nix {inherit pkgs;})
    iconv # not sure why
    lima
    # neovide # build fails
    # nixos-generators
    # php-with-extensions # remove?
    qemu
  ];

  security.pam.services.sudo_local.touchIdAuth = true;

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

  system.stateVersion = 4;

  # nix-darwin is working on multi-user support
  system.primaryUser = "johnny";

  users.users."johnny" = {
    shell = pkgs.zsh;
    # https://github.com/LnL7/nix-darwin/issues/682
    home = "/Users/johnny";
  };
}
