{pkgs, ...}: {
  home.packages = with pkgs; [
    ungoogled-chromium
  ];

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
    "text/csv" = "calc.desktop";
  };
  xdg.mimeApps.associations.added = {
    "text/csv" = "calc.desktop";
    "image/jpeg" = "oculante.desktop;org.gnome.Loupe.desktop;vimiv.desktop;satty.desktop;org.photoqt.PhotoQt.desktop";
  };
}
