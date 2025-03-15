{config, pkgs, ...}: {
  system.activationScripts.postUserActivation.text = ''
    rsyncArgs=("--archive" "--checksum" "--chmod=-w" "--copy-unsafe-links" "--delete")
    apps_source="${config.system.build.applications}/Applications"
    moniker="Nix Trampolines"
    app_target_base="$HOME/Applications"
    app_target="$app_target_base/$moniker"
    mkdir -p "$app_target"
    ${pkgs.rsync}/bin/rsync "''${rsyncArgs[@]}" "$apps_source/" "$app_target"
  '';
}
