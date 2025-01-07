{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../common/presets/nixos.nix
    ../common/networks
    ./amdgpu.nix
    ./hardware-configuration.nix
    ./clamav.nix
    ./dns.nix
    ./firefox.nix
    ./hyprland.nix
    ./keyboard.nix
    ./nfs.nix
    ./nix-ld.nix
    ./wireguard.nix
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

  networking.hostName = "iota";

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

  services.avahi.enable = true;
  services.avahi.nssmdns4 = true;

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
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

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
  };
  programs.noisetorch.enable = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = [];
  environment.systemPackages = with pkgs; [
    samba
    tree
  ];

  system.stateVersion = "24.05";
}
