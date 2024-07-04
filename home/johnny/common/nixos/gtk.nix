{...}: {
  dconf.settings = {
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
