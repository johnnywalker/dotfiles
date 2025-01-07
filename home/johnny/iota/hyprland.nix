{
  inputs,
  pkgs,
  ...
}: let
  hyprland-pkgs = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system};
  pkgs-unstable = inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in {
  # hint Electron apps to use Wayland:
  home.sessionVariables.NIXOS_OZONE_WL = "1";
  home.sessionVariables.XCURSOR_SIZE = "24";
  home.sessionVariables.HYPRCURSOR_SIZE = "24";
  home.sessionVariables.GRIMBLAST_EDITOR = "${pkgs.swappy}/bin/swappy -f";

  programs.zsh.profileExtra = ''
    if uwsm check may-start && uwsm select; then
      exec systemd-cat -t uwsm_start uwsm start default
    fi
  '';

  # TODO configure hyprswitch
  # TODO configure hyprexpo
  home.packages = with pkgs; [
    cliphist
    fuzzel
    grim
    grimblast
    mako
    pavucontrol
    slurp
    wlogout
  ];

  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    colors = {
      background = "111316f0";
      text = "888888ff";
      selection = "888888ff";
      "selection-text" = "111316f0";
      border = "111316ff";
    };
    border.width = 1;
  };

  services.cliphist.enable = true;
  systemd.user.services.cliphist.Unit.After = ["graphical-session.target"];
  systemd.user.services.cliphist-images.Unit.After = ["graphical-session.target"];

  programs.kitty.enable = true;
  # match mesa version if using hyprland flake
  # programs.kitty.package = pkgs-unstable.kitty;
  wayland.windowManager.hyprland.enable = true;
  # wayland.windowManager.hyprland.package = hyprland-pkgs.hyprland;
  wayland.windowManager.hyprland.plugins = [
    # inputs.hyprspace.packages.${pkgs.stdenv.hostPlatform.system}.Hyprspace
    pkgs.hyprlandPlugins.hyprspace
    # this doesn't seem to work
    # pkgs-unstable.hyprlandPlugins.hyprspace
  ];
  # use UWSM
  wayland.windowManager.hyprland.systemd.enable = false;
  wayland.windowManager.hyprland.settings = {
    "$terminal" = "uwsm app -- kitty";
    "$fileManager" = "uwsm app -- thunar";
    "$menu" = "uwsm app -- fuzzel";
    "$logout" = "uwsm app -- wlogout";
    "$cliphist_list" = "uwsm app -- cliphist list | fuzzel --dmenu | cliphist decode | wl-copy";

    # debug = {
    #   disable_logs = false;
    #   enable_stdout_logs = true;
    # };

    # auto-start
    exec-once = [
      "uwsm app -- firefox"
      "uwsm app -- mako"
      # "${pkgs.networkmanagerapplet}/bin/nm-applet"
      "uwsm app -- teams-for-linux"
    ];

    # https://wiki.hyprland.org/Configuring/Variables/#general
    general = {
      gaps_in = 5;
      gaps_out = 20;

      border_size = 2;

      # https://wiki.hyprland.org/Configuring/Variables/#variable-types for info about colors
      "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
      "col.inactive_border" = "rgba(595959aa)";

      # Set to true enable resizing windows by clicking and dragging on borders and gaps
      resize_on_border = false;

      # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
      allow_tearing = false;

      layout = "dwindle";
    };

    # https://wiki.hyprland.org/Configuring/Variables/#decoration
    decoration = {
      rounding = 10;

      # Change transparency of focused and unfocused windows
      active_opacity = 1.0;
      inactive_opacity = 1.0;

      shadow = {
        enabled = true;
        color = "rgba(1a1a1aee)";
        range = 4;
        render_power = 3;
      };

      # https://wiki.hyprland.org/Configuring/Variables/#blur
      blur = {
        enabled = true;
        size = 3;
        passes = 1;

        vibrancy = 0.1696;
      };
    };

    # https://wiki.hyprland.org/Configuring/Variables/#animations
    animations = {
      enabled = true;

      # Default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

      bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";

      animation = [
        "windows, 1, 7, myBezier"
        "windowsOut, 1, 7, default, popin 80%"
        "border, 1, 10, default"
        "borderangle, 1, 8, default"
        "fade, 1, 7, default"
        "workspaces, 1, 6, default"
      ];
    };

    # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
    dwindle = {
      pseudotile = true; # Master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
      preserve_split = true; # You probably want this
    };

    # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
    master = {
      new_status = "master";
    };

    # https://wiki.hyprland.org/Configuring/Variables/#misc
    misc = {
      force_default_wallpaper = -1; # Set to 0 or 1 to disable the anime mascot wallpapers
      disable_hyprland_logo = true; # If true disables the random hyprland logo / anime girl background. :(
    };

    # https://wiki.hyprland.org/Configuring/Variables/#input
    input = {
      kb_layout = "us";
      kb_variant = "";
      kb_model = "";
      # kb_options = altwin:swap_alt_win
      kb_rules = "";

      follow_mouse = 1;

      sensitivity = 0; # -1.0 - 1.0, 0 means no modification.

      touchpad = {
        natural_scroll = false;
      };
    };

    # https://wiki.hyprland.org/Configuring/Variables/#gestures
    gestures = {
      workspace_swipe = false;
    };

    # See https://wiki.hyprland.org/Configuring/Keywords/
    "$mainMod" = "SUPER"; # Sets "Windows" key as main modifier

    # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
    bind = [
      # "$mainMod, Q, exec, $terminal"
      # "$mainMod, T, exec, $terminal"
      "$mainMod, grave, exec, $terminal" # grave accent
      # "$mainMod, C, killactive,"
      "$mainMod, W, killactive,"
      # "$mainMod, M, exit,"
      "$mainMod, M, exec, $logout"
      "$mainMod, E, exec, $fileManager"
      "$mainMod SHIFT, F, togglefloating,"
      # "$mainMod, R, exec, $menu"
      "$mainMod, SPACE, exec, $menu"
      "$mainMod, P, pseudo," # dwindle
      "$mainMod, J, togglesplit," # dwindle
      # "$mainMod, L, exec, hyprlock"
      "$mainMod, L, exec, loginctl lock-session"

      # Screenshot binds
      ", Print, exec, uwsm app -- uwsm app -- grimblast --notify copy output" # screenshot to clipboard
      "CTRL, Print, exec, uwsm app -- grimblast --notify save area" # snip area and save to file
      "SHIFT, Print, exec, uwsm app -- grimblast --notify copy area" # snip area and copy to clipboard
      "$mainMod, Print, exec, uwsm app -- grimblast edit area" # snip area and edit

      # display cliphist
      "$mainMod, V, exec, $cliphist_list"

      # Move focus with mainMod + arrow keys
      "$mainMod, left, movefocus, l"
      "$mainMod, right, movefocus, r"
      "$mainMod, up, movefocus, u"
      "$mainMod, down, movefocus, d"

      # Switch workspaces with mainMod + [0-9]
      "$mainMod, 1, workspace, 1"
      "$mainMod, 2, workspace, 2"
      "$mainMod, 3, workspace, 3"
      "$mainMod, 4, workspace, 4"
      "$mainMod, 5, workspace, 5"
      "$mainMod, 6, workspace, 6"
      "$mainMod, 7, workspace, 7"
      "$mainMod, 8, workspace, 8"
      "$mainMod, 9, workspace, 9"
      "$mainMod, 0, workspace, 10"

      # Show workspace overview
      "$mainMod CTRL, up, overview:toggle,"

      # Move active window to a workspace with mainMod + SHIFT + [0-9]
      "$mainMod SHIFT, 1, movetoworkspace, 1"
      "$mainMod SHIFT, 2, movetoworkspace, 2"
      "$mainMod SHIFT, 3, movetoworkspace, 3"
      "$mainMod SHIFT, 4, movetoworkspace, 4"
      "$mainMod SHIFT, 5, movetoworkspace, 5"
      "$mainMod SHIFT, 6, movetoworkspace, 6"
      "$mainMod SHIFT, 7, movetoworkspace, 7"
      "$mainMod SHIFT, 8, movetoworkspace, 8"
      "$mainMod SHIFT, 9, movetoworkspace, 9"
      "$mainMod SHIFT, 0, movetoworkspace, 10"

      # Example special workspace (scratchpad)
      "$mainMod, S, togglespecialworkspace, magic"
      "$mainMod SHIFT, S, movetoworkspace, special:magic"

      # Scroll through existing workspaces with mainMod + scroll
      "$mainMod, mouse_down, workspace, e-1"
      "$mainMod, mouse_up, workspace, e+1"

      # Scroll through existing workspaces with mainMod + CTRL + left/right
      "$mainMod CTRL, left, workspace, e-1"
      "$mainMod CTRL, right, workspace, e+1"

      # Toggle window group with mainMod + G
      "$mainMod, G, togglegroup,"

      # Move focus within group using mainMod + ALT + arrow keys
      "$mainMod ALT, up, changegroupactive, b"
      "$mainMod ALT, down, changegroupactive, f"

      # Move windows in/out of group with mainMod + SHIFT + arrow keys
      "$mainMod SHIFT, left, movewindoworgroup, l"
      "$mainMod SHIFT, right, movewindoworgroup, r"
      "$mainMod SHIFT, up, movewindoworgroup, u"
      "$mainMod SHIFT, down, movewindoworgroup, d"
    ];

    # Move/resize windows with mainMod + LMB/RMB and dragging
    bindm = [
      "$mainMod, mouse:272, movewindow"
      "$mainMod, mouse:273, resizewindow"
    ];

    # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
    # See https://wiki.hyprland.org/Configuring/Workspace-Rules/ for workspace rules

    # Example windowrule v1
    # windowrule = "float, ^(kitty)$";

    # Example windowrule v2
    # windowrulev2 = "float,class:^(kitty)$,title:^(kitty)$";

    windowrulev2 = [
      "suppressevent maximize, class:.*" # You'll probably like this.
    ];
  };

  programs.hyprlock.enable = true;
  # programs.hyprlock.package = pkgs-unstable.hyprlock;

  services.hypridle.enable = true;
  # services.hypridle.package = pkgs-unstable.hypridle;
  services.hypridle.settings = {
    general = {
      before_sleep_cmd = "loginctl lock-session"; # lock before suspend.
      after_sleep_cmd = "hyprctl dispatch dpms on"; # to avoid having to press a key twice to turn on the display.
      ignore_dbus_inhibit = false;
      lock_cmd = "pidof hyprlock || hyprlock"; # avoid starting multiple hyprlock instances.
    };

    listener = [
      # getting red screen of death on 0.41.2
      # {
      #   timeout = 900; # 10 min
      #   on-timeout = "loginctl lock-session"; # lock screen when timeout has passed
      # }
      {
        timeout = 930; # 10.5 min
        on-timeout = "hyprctl dispatch dpms off";
        on-resume = "hyprctl dispatch dpms on";
      }
    ];
  };
  systemd.user.services.hypridle.Unit.After = ["graphical-session.target"];

  gtk = {
    enable = true;
    iconTheme = {
      name = "elementary-Xfce-dark";
      package = pkgs.elementary-xfce-icon-theme;
    };
    theme = {
      # name = "zukitre-dark";
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
}
