{pkgs, ...}: {
  xdg.desktopEntries.noise = {
    name = "Noise";
    genericName = "Noise";
    exec = ''
      sh -c "${pkgs.sox}/bin/play -n synth brownnoise pinknoise mix"
    '';
    terminal = true;
    icon = "utilities-terminal";
    categories = ["AudioVideo"];
    type = "Application";
  };
}
