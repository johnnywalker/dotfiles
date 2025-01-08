{...}: {
  dconf.settings = {
    "org/gnome/desktop/screensaver" = {
      "lock-delay" = "uint32 30";
    };
    "org/gnome/desktop/session" = {
      "idle-delay" = "uint32 600";
    };
    "org/gnome/settings-daemon/plugins/color" = {
      "night-light-enabled" = true;
      "night-light-schedule-automatic" = true;
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Control>grave";
      command = "guake-toggle";
      name = "guake-toggle";
    };
  };
}
