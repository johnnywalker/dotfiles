{...}: {
  # ncurses client
  programs.ncmpcpp.enable = true;
  programs.ncmpcpp.bindings = [
    {
      key = "j";
      command = "scroll_down";
    }
    {
      key = "k";
      command = "scroll_up";
    }
    {
      key = "J";
      command = ["select_item" "scroll_down"];
    }
    {
      key = "K";
      command = ["select_item" "scroll_up"];
    }
  ];

  services.mpd = {
    enable = true;
    extraConfig = ''
      audio_output {
        type "pipewire"
        name "PipeWire"
      }
    '';
    musicDirectory = "/mnt/music";
    # network.startWhenNeeded = true;
  };

  services.mpdris2.enable = true;
}
