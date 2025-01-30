{pkgs, ...}: {
  # hint Electron apps to use Wayland:
  home.sessionVariables.NIXOS_OZONE_WL = "1";

  qt = {
    enable = true;
    platformTheme.name = "gtk3";
  };
  gtk = {
    enable = true;
    cursorTheme = {
      name = "Nordzy-cursors";
      # use unstable to get hyprcursors as well
      package = pkgs.unstable.nordzy-cursor-theme;
    };
    iconTheme = {
      name = "elementary-xfce-dark";
      package = pkgs.elementary-xfce-icon-theme;
    };
    theme = {
      # name = "Zukitre-dark";
      # package = pkgs.zuki-themes;
      name = "Nordic";
      package = pkgs.nordic;
    };
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  programs.kitty.enable = true;
  programs.waybar.enable = true;
  programs.fuzzel.enable = true;
  services.cliphist.enable = true;
}
