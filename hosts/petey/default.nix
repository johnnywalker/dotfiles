{config, ...}: {
  imports = [
    ../common/presets/nixos.nix
    ../common/networks
    ../common/users/henry
    ./hardware-configuration.nix
    ./dns.nix
    ./firefox.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "petey";

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = config.services.openssh.ports;

  # networking.useDHCP = true;

  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.
  networking.wireless.environmentFile = config.sops.secrets.wireless-env.path;
  networking.wireless.networks = {
    RuggedBits.psk = "@RUGGEDBITS_PSK@";
  };

  # Disable networkmanager
  networking.networkmanager.enable = false;

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

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  environment.systemPackages = [];

  system.stateVersion = "24.05";
}
