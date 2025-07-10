{config, lib, pkgs, ...}: {
  # https://github.com/nix-darwin/nix-darwin/issues/214#issuecomment-2467550779
  # this variant no longer works due to postUserActivation removal in 25.05
  # system.activationScripts.postUserActivation.text = ''
  #   rsyncArgs=("--archive" "--checksum" "--chmod=-w" "--copy-unsafe-links" "--delete")
  #   apps_source="${config.system.build.applications}/Applications"
  #   moniker="Nix Trampolines"
  #   app_target_base="$HOME/Applications"
  #   app_target="$app_target_base/$moniker"
  #   mkdir -p "$app_target"
  #   ${pkgs.rsync}/bin/rsync "''${rsyncArgs[@]}" "$apps_source/" "$app_target"
  # '';

  # until https://github.com/nix-darwin/nix-darwin/pull/1396 is merged
  system.activationScripts.applications.text = ''
    # Set up applications.
    echo "setting up /Applications/Nix Apps..." >&2

    ourLink () {
      local link
      link=$(readlink "$1")
      [ -L "$1" ] && [ "''${link#*-}" = 'system-applications/Applications' ]
    }

    ${lib.optionalString (config.system.primaryUser != null) ''
      # Clean up for links created at the old location in HOME
      # TODO: Remove this in 25.11.
      if ourLink ~${config.system.primaryUser}/Applications; then
        rm ~${config.system.primaryUser}/Applications
      elif ourLink ~${config.system.primaryUser}/Applications/'Nix Apps'; then
        rm ~${config.system.primaryUser}/Applications/'Nix Apps'
      fi
    ''}

    targetFolder='/Applications/Nix Apps'

    # Clean up old style symlink to nix store
    if [ -e "$targetFolder" ] && ourLink "$targetFolder"; then
      rm "$targetFolder"
    fi

    mkdir -p "$targetFolder"

    rsyncFlags=(
      # mtime is standardized in the nix store, which would leave only file size to distinguish files.
      # Thus we need checksums, despite the speed penalty.
      --checksum
      # Converts all symlinks pointing outside of the copied tree (thus unsafe) into real files and directories.
      # This neatly converts all the symlinks pointing to application bundles in the nix store into
      # real directories, without breaking any relative symlinks inside of application bundles.
      # This is good enough, because the make-symlinks-relative.sh setup hook converts all $out internal
      # symlinks to relative ones.
      --copy-unsafe-links
      --archive
      --delete
      --chmod=-w
      --no-group
      --no-owner
    )

    ${lib.getExe pkgs.rsync} "''${rsyncFlags[@]}" ${config.system.build.applications}/Applications/ "$targetFolder"
  '';
}
