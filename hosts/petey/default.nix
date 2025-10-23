{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../common/presets/nixos.nix
    ../common/users/henry
    ./hardware-configuration.nix
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

  # Disable networkmanager
  networking.networkmanager.enable = false;

  # auto-login breaks gdm - think it's a getty conflict
  # services.displayManager.autoLogin.enable = true;
  # services.displayManager.autoLogin.user = "henry";

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Define custom session for launching educational applications.
  # services.xserver.displayManager.session = [
  #   {
  #     manage = "desktop";
  #     name = "step1";
  #     start = ''
  #       chosen=$(<<<'GCompris
  #       Super Mario Bros
  #       Super Tux Kart
  #       Tux Typing' ${pkgs.rofi}/bin/rofi -dmenu -i -format i)
  #       case "$chosen" in
  #       0) ${pkgs.gcompris}/bin/gcompris-qt & ;;
  #       1) ${pkgs.mari0}/bin/mari0 & ;;
  #       2) ${pkgs.superTuxKart}/bin/supertuxkart & ;;
  #       3) ${pkgs.tuxtype}/bin/tuxtype & ;;
  #       esac
  #       waitPID=$!
  #     '';
  #   }
  # ];

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
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
