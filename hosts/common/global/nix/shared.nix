{
  pkgs,
  lib,
  inputs,
  ...
}: {
  nixpkgs = {
    overlays = [
      # Any additional overlays can be placed here
      (final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          system = final.system;
          config = final.config;
        };

        # master = import inputs.nixpkgs-master {
        #   system = final.system;
        #   config = final.config;
        # };
      })
    ];
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    extraOptions = ''
      plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins
      # keep-outputs = true
      # keep-derivations = true
    '';

    package = pkgs.nix;

    settings = {
      # https://github.com/NixOS/nix/issues/7273
      auto-optimise-store = pkgs.stdenv.isLinux;
      experimental-features = ["nix-command" "flakes" "repl-flake"];
      # keep-derivations = true;
      # keep-outputs = true;
      trusted-users =
        ["root"]
        ++ lib.optional pkgs.stdenvNoCC.isLinux "@wheel"
        ++ lib.optional pkgs.stdenvNoCC.isDarwin "@admin";
      # min-free = lib.mkDefault (10 * 1000 * 1000 * 1000); # 10gb
      cores = lib.mkDefault 0;
      max-jobs = lib.mkDefault "auto";
    };
  };
}
