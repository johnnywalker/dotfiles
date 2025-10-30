{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../common/presets/nixos.nix
    ./hardware-configuration.nix
    ./clamav.nix
    ./firefox.nix
    ./hyprland.nix
    ./nix-ld.nix
    ./sway.nix
    ./wayland.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 2;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "m3800";

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = config.services.openssh.ports;

  # networking.useDHCP = true;

  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  # Disable networkmanager
  networking.networkmanager.enable = false;

  services.logind.lidSwitch = "hybrid-sleep";

  programs.thunar.enable = true;
  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-media-tags-plugin
    thunar-volman
  ];
  # enable network browsing in thunar
  services.gvfs = {
    enable = true;
    # use version with samba support
    package = lib.mkForce pkgs.gnome.gvfs;
  };
  services.tumbler.enable = true; # Thumbnail support for images

  # Enable the X11 windowing system.
  services.xserver.enable = false;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # TODO configure resolution and scaling (100%)
  # # Reduce resolution from 3840x2160 to 1920x1080
  # services.xserver.xrandrHeads = ["eDP1"];
  # services.xserver.resolutions = [
  #   {
  #     x = 1920;
  #     y = 1080;
  #   }
  # ];

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # this might not be required
  # environment.shells = with pkgs; [
  #   bashInteractive
  #   zsh
  # ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    lm_sensors
    tree
  ];

  system.stateVersion = "24.05";
}
