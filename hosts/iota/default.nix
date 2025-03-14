{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../common/presets/nixos.nix
    ../common/users/jonjon
    ./amdgpu.nix
    ./hardware-configuration.nix
    ./clamav.nix
    ./firefox.nix
    ./hyprland.nix
    ./keyboard.nix
    ./nix-ld.nix
    ./llm.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel to support X870 motherboard
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.luks.devices."crypted" = {
    device = "/dev/disk/by-uuid/6a54cf6e-af61-4f3b-aad9-f0d24b9ea6ce";
    preLVM = true;
    allowDiscards = true;
  };

  # support cross-compiling for ARM64
  boot.binfmt.emulatedSystems = ["aarch64-linux"];

  networking.hostName = "iota";

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = config.services.openssh.ports;
  networking.firewall.allowedUDPPorts = [
    5353 # mDNS
  ];

  # networking.useDHCP = true;

  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  # Disable networkmanager
  networking.networkmanager.enable = false;

  gtk.iconCache.enable = true;

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

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [canon-cups-ufr2];
  hardware.printers.ensurePrinters = [
    {
      name = "MF642CDW-UFR-II";
      location = "Home";
      deviceUri = "ipp://[2600:1700:58b9:a01f:1298:c3ff:fee0:a418]/ipp/print";
      model = "CNRCUPSMF642CZS.ppd";
    }
    {
      name = "MF642CDW-IPP-Everywhere";
      location = "Home";
      deviceUri = "ipp://[2600:1700:58b9:a01f:1298:c3ff:fee0:a418]/ipp/print";
      model = "everywhere";
    }
  ];

  hardware.sane.enable = true;
  # hardware.sane.netConf = "[2600:1700:58b9:a01f:1298:c3ff:fee0:a418]";
  hardware.sane.openFirewall = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  programs.noisetorch.enable = true;

  services.cpupower-gui.enable = true;

  services.tailscale.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = [];
  environment.systemPackages = with pkgs; [
    lm_sensors
    samba
    simple-scan
    tree
  ];

  system.stateVersion = "24.05";
}
