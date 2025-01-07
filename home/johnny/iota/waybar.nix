{...}: {
  programs.waybar.enable = true;
  programs.waybar.systemd.enable = true;
  # https://github.com/NixOS/nixpkgs/issues/347651
  systemd.user.services.waybar.Unit.After = ["graphical-session.target"];
}
