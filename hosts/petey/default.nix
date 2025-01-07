{
  config,
  pkgs,
  ...
}: {
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
  networking.wireless.secretsFile = config.sops.secrets.wireless-secrets.path;
  networking.wireless.networks = {
    RuggedBits.pskRaw = "ext:psk_ruggedbits";
  };

  # Disable networkmanager
  networking.networkmanager.enable = false;

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "henry";

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Define custom session for launching educational applications.
  services.xserver.displayManager.session = [
    {
      manage = "desktop";
      name = "step1";
      start = ''
        ${pkgs.gcompris}/bin/gcompris-qt &
        waitPID=$!
      '';
    }
  ];

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

  environment.systemPackages = with pkgs; [
    alsa-utils
    gcompris
    tuxtype
  ];

  system.stateVersion = "24.05";
}
