{
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager

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
      cmake # required to build some emacs packages
      emacs29-pgtk
      gh
      guake
      neovide
      neovim-gtk
      neovim-qt
      nerdfonts
      psmisc
      ripgrep
      vim
      wl-clipboard
    ];
  };

  programs.zsh.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  system = {
    stateVersion = lib.mkDefault "24.05";
  };
}
