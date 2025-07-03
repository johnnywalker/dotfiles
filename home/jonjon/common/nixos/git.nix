{pkgs, ...}: {
  # Use gpg2 with pinentry-mode loopback when executed from Emacs or on a
  # headless system
  programs.git.signing.signer = let
    gpg2-display-aware = pkgs.writeScriptBin "gpg2-display-aware" ''
      #!${pkgs.runtimeShell}
      if [[ -n "$INSIDE_EMACS" ]] || [[ -z "$WAYLAND_DISPLAY" && -z "$DISPLAY" ]]; then
        exec ${pkgs.gnupg}/bin/gpg2 --pinentry-mode loopback "$@"
      else
        exec ${pkgs.gnupg}/bin/gpg2 "$@"
      fi
    '';
  in "${gpg2-display-aware}/bin/gpg2-display-aware";
}
