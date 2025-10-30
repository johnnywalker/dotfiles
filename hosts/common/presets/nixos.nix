{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../global/nix/nixos.nix
    ../global/docker.nix
    ../global/home-manager.nix
    ../global/locale.nix
    ../global/oom-killer.nix
    ../global/openssh.nix

    ../users/johnny
  ];

  boot.tmp.cleanOnBoot = true;

  sops = {
    age = {
      generateKey = true;
      keyFile = "/var/lib/sops-nix/key.txt";
      sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
    };
  };

  environment = {
    # pathsToLink = ["/share" "/bin"];
    systemPackages = with pkgs; [
      bitwarden-desktop
      btop
      gh
      neovim-gtk
      neovim-qt
      psmisc
      ripgrep
      vim
      wl-clipboard
      xdg-launch
    ];
  };

  fonts.enableDefaultPackages = true;
  fonts.packages = with pkgs; [
    emacs-all-the-icons-fonts
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    noto-fonts
    noto-fonts-emoji
    roboto
    source-code-pro
    source-sans-pro
    source-serif-pro
  ];
  fonts.fontconfig.subpixel.rgba = "rgb";

  programs.zsh.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    settings = {
      allow-loopback-pinentry = "";
      allow-emacs-pinentry = "";
    };
  };

  system = {
    stateVersion = lib.mkDefault "24.05";
  };
}
