{
  config,
  lib,
  ...
}: {
  # hint Electron apps to use Wayland:
  home.sessionVariables.NIXOS_OZONE_WL = "1";

  programs.kitty.enable = true;
  programs.waybar.enable = true;
  programs.fuzzel.enable = true;
  services.cliphist.enable = true;

  wayland.windowManager.sway = {
    enable = true;
    config = {
      modifier = "Mod4";
      keybindings = let
        modifier = config.wayland.windowManager.sway.config.modifier;
      in
        lib.mkOptionDefault {
          "${modifier}+grave" = "exec kitty";
          "${modifier}+e" = "exec thunar";
          "${modifier}+space" = "exec fuzzel";
          "${modifier}+v" = "exec cliphist list | fuzzel --dmenu | cliphist decode | wl-copy";
          "XF86MonBrightnessDown" = "exec light -U 5";
          "XF86MonBrightnessUp" = "exec light -A 5";
          "XF86AudioRaiseVolume" = "exec 'pactl set-sink-volume @DEFAULT_SINK@ +1%'";
          "XF86AudioLowerVolume" = "exec 'pactl set-sink-volume @DEFAULT_SINK@ -1%'";
          "XF86AudioMute" = "exec 'pactl set-sink-mute @DEFAULT_SINK@ toggle'";
        };
      gaps = {
        inner = 10;
      };
      gaps.smartBorders = "on";
      gaps.smartGaps = true;
      input = {
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
        };
      };
      # Use kitty as default terminal
      terminal = "kitty";
      startup = [
        # Launch Firefox on start
        {command = "firefox";}
      ];
    };
  };
}
