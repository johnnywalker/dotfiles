{
  inputs,
  pkgs,
  ...
}: {
  home.packages = [
    inputs.hyprpolkitagent.packages."${pkgs.stdenv.hostPlatform.system}".hyprpolkitagent
  ];
  # start polkit to support authentication prompts
  wayland.windowManager.hyprland.settings.exec-once = [
    "systemctl --user start hyprpolkitagent"
  ];
}
