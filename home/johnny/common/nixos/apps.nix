{pkgs, ...}: {
  home.packages = with pkgs; [
    bitwarden-cli
    evince
    ftop # monitor progress of open files
    inkscape
    nethogs # "net top" tool
    ungoogled-chromium
  ];

  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    # prevent chromium from opening by default
    "application/msword" = "writer.desktop";
    "application/mxf" = "vlc.desktop";
    "application/ogg" = "vlc.desktop";
    "application/prs.plucker" = "writer.desktop";
    "application/ram" = "vlc.desktop";
    "application/rtf" = "writer.desktop";
    "application/sdp" = "vlc.desktop";
    "application/vnd.adobe.flash.movie" = "vlc.desktop";
    "application/vnd.apple.mpegurl" = "vlc.desktop";
    "application/vnd.apple.numbers" = "calc.desktop";
    "application/vnd.apple.pages" = "writer.desktop";
    "application/vnd.dbf" = "calc.desktop";
    "application/vnd.efi.iso" = "vlc.desktop";
    "application/vnd.lotus-1-2-3" = "calc.desktop";
    "application/vnd.lotus-wordpro" = "writer.desktop";
    "application/vnd.ms-asf" = "vlc.desktop";
    "application/vnd.ms-excel" = "calc.desktop";
    "application/vnd.ms-works" = "writer.desktop";
    "application/vnd.ms-wpl" = "vlc.desktop";
    "application/vnd.oasis.opendocument.chart" = "calc.desktop";
    "application/vnd.oasis.opendocument.chart-template" = "calc.desktop";
    "application/vnd.oasis.opendocument.spreadsheet" = "calc.desktop";
    "application/vnd.oasis.opendocument.spreadsheet-flat-xml" = "calc.desktop";
    "application/vnd.oasis.opendocument.spreadsheet-template" = "calc.desktop";
    "application/vnd.oasis.opendocument.text-flat-xml" = "writer.desktop";
    "application/vnd.oasis.opendocument.text-master" = "writer.desktop";
    "application/vnd.oasis.opendocument.text-template" = "writer.desktop";
    "application/vnd.oasis.opendocument.text-web" = "writer.desktop";
    "application/vnd.oasis.opendocument.text" = "writer.desktop";
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "calc.desktop";
    "application/vnd.openxmlformats-officedocument.spreadsheetml.template" = "calc.desktop";
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "writer.desktop";
    "application/vnd.openxmlformats-officedocument.wordprocessingml.template" = "writer.desktop";
    "application/vnd.palm" = "writer.desktop";
    "application/vnd.rn-realmedia" = "vlc.desktop";
    "application/vnd.stardivision.calc" = "calc.desktop";
    "application/vnd.stardivision.chart" = "calc.desktop";
    "application/vnd.stardivision.writer" = "writer.desktop";
    "application/vnd.sun.xml.calc" = "calc.desktop";
    "application/vnd.sun.xml.calc.template" = "calc.desktop";
    "application/vnd.sun.xml.writer.global" = "writer.desktop";
    "application/vnd.sun.xml.writer.template" = "writer.desktop";
    "application/vnd.sun.xml.writer" = "writer.desktop";
    "application/vnd.wordperfect" = "writer.desktop";
    "application/x-abiword" = "writer.desktop";
    "application/x-docbook+xml" = "writer.desktop";
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/x-fictionbook+xml" = "writer.desktop";
    "application/x-gnumeric" = "calc.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "application/x-hwp" = "writer.desktop";
    "application/x-matroska" = "vlc.desktop";
    "application/x-mswrite" = "writer.desktop";
    "application/x-pocket-word" = "writer.desktop";
    "application/x-quattropro" = "calc.desktop";
    "application/x-shorten" = "vlc.desktop";
    "application/xspf+xml" = "vlc.desktop";
    "application/x-t602" = "writer.desktop";
    "audio/aac" = "vlc.desktop";
    "audio/ac3" = "vlc.desktop";
    "audio/AMR" = "vlc.desktop";
    "audio/AMR-WB" = "vlc.desktop";
    "audio/basic" = "vlc.desktop";
    "audio/flac" = "vlc.desktop";
    "audio/midi" = "vlc.desktop";
    "audio/mp2" = "vlc.desktop";
    "audio/mp4" = "vlc.desktop";
    "audio/mpeg" = "vlc.desktop";
    "audio/vnd.dts" = "vlc.desktop";
    "audio/vnd.rn-realaudio" = "vlc.desktop";
    "audio/vnd.wave" = "vlc.desktop";
    "audio/x-adpcm" = "vlc.desktop";
    "audio/x-aiff" = "vlc.desktop";
    "audio/x-ape" = "vlc.desktop";
    "audio/x-gsm" = "vlc.desktop";
    "audio/x-it" = "vlc.desktop";
    "audio/x-mod" = "vlc.desktop";
    "audio/x-mpegurl" = "vlc.desktop";
    "audio/x-ms-asx" = "vlc.desktop";
    "audio/x-musepack" = "vlc.desktop";
    "audio/x-s3m" = "vlc.desktop";
    "audio/x-scpls" = "vlc.desktop";
    "audio/x-speex" = "vlc.desktop";
    "audio/x-tta" = "vlc.desktop";
    "audio/x-wavpack" = "vlc.desktop";
    "audio/x-xm" = "vlc.desktop";
    "font/ttf" = "com.github.FontManager.FontViewer.desktop";
    "image/avif" = "org.gnome.Loupe.desktop";
    "image/bmp" = "org.gnome.Loupe.desktop";
    "image/gif" = "org.gnome.Loupe.desktop";
    "image/heif" = "org.gnome.Loupe.desktop";
    "image/jpeg" = "org.gnome.Loupe.desktop";
    "image/jxl" = "org.gnome.Loupe.desktop";
    "image/png" = "org.gnome.Loupe.desktop";
    "image/svg+xml-compressed" = "org.gnome.Loupe.desktop";
    "image/svg+xml" = "org.gnome.Loupe.desktop";
    "image/tiff" = "org.gnome.Loupe.desktop";
    "image/vnd.microsoft.icon" = "org.gnome.Loupe.desktop";
    "image/webp" = "org.gnome.Loupe.desktop";
    "image/x-dds" = "org.gnome.Loupe.desktop";
    "image/x-exr" = "org.gnome.Loupe.desktop";
    "image/x-portable-anymap" = "org.gnome.Loupe.desktop";
    "image/x-tga" = "org.gnome.Loupe.desktop";
    "inode/directory" = "thunar.desktop";
    "text/csv" = "calc.desktop";
    "text/html" = "firefox.desktop";
    "text/plain" = "nvim-qt.desktop";
    "video/3gpp2" = "vlc.desktop";
    "video/dv" = "vlc.desktop";
    "video/mp2t" = "vlc.desktop";
    "video/mp4" = "vlc.desktop";
    "video/mpeg" = "vlc.desktop";
    "video/quicktime" = "vlc.desktop";
    "video/vnd.avi" = "vlc.desktop";
    "video/vnd.mpegurl" = "vlc.desktop";
    "video/vnd.rn-realvideo" = "vlc.desktop";
    "video/webm" = "vlc.desktop";
    "video/x-anim" = "vlc.desktop";
    "video/x-flic" = "vlc.desktop";
    "video/x-flv" = "vlc.desktop";
    "video/x-nsv" = "vlc.desktop";
    "x-scheme-handler/chrome" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
    "x-scheme-handler/jetbrains" = "jetbrains-toolbox.desktop";
    "x-scheme-handler/msteams" = "teams-for-linux.desktop";
  };
  xdg.mimeApps.associations.added = {
    "image/jpeg" = "oculante.desktop;org.gnome.Loupe.desktop;vimiv.desktop;satty.desktop;org.photoqt.PhotoQt.desktop";
    "image/png" = "oculante.desktop;org.gnome.Loupe.desktop;vimiv.desktop;satty.desktop;org.photoqt.PhotoQt.desktop";
    "text/csv" = "calc.desktop";
  };

  xdg.configFile."xfce4/helpers.rc".text = ''
    TerminalEmulator=kitty
  '';
}
