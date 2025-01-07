{pkgs, ...}: {
  home.packages = with pkgs; [
    jetbrains-toolbox
    ungoogled-chromium
  ];

  # toolbox attempts to add its own autostart entry, but we need to wrapped version for NixOS
  xdg.configFile."autostart/jetbrains-toolbox.desktop".source = "${pkgs.jetbrains-toolbox}/share/applications/jetbrains-toolbox.desktop";

  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    # prevent chromium from opening by default
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "text/html" = "firefox.desktop";
    "x-scheme-handler/chrome" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
    "x-scheme-handler/jetbrains" = "jetbrains-toolbox.desktop";
    "text/csv" = "calc.desktop";
  };
  xdg.mimeApps.associations.added = {
    "text/csv" = "calc.desktop";
    "image/jpeg" = "oculante.desktop;org.gnome.Loupe.desktop;vimiv.desktop;satty.desktop;org.photoqt.PhotoQt.desktop";
  };

  xdg.configFile."xfce4/helpers.rc".text = ''
    TerminalEmulator=kitty
  '';
}
