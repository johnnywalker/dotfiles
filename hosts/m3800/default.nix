{pkgs, ...}: {
  imports = [
    ../common/presets/nixos.nix
    ./hardware-configuration.nix
    ./dns.nix
    ./firefox.nix
    ./nix-ld.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "m3800";

  networking.firewall.enable = true;
  # networking.useDHCP = true;

  # TODO
  # - configure wireless
  # - configure JetBrains Mono font for terminal (including Guake)

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

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
  hardware.pulseaudio.enable = false;
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
  environment.systemPackages = [];

  system.stateVersion = "24.05";
}
