{pkgs, ...}: {
  home.packages = with pkgs; [
    jetbrains-toolbox
  ];

  # toolbox attempts to add its own autostart entry, but we need to wrapped version for NixOS
  xdg.configFile."autostart/jetbrains-toolbox.desktop".source = "${pkgs.jetbrains-toolbox}/share/applications/jetbrains-toolbox.desktop";
}
