{
  pkgs,
  lib,
  ...
}: {
  nix = {
    extraOptions = ''
      # keep-outputs = true
      # keep-derivations = true
    '';

    package = pkgs.nix;

    settings = {
      # https://github.com/NixOS/nix/issues/7273
      # auto-optimise-store = pkgs.stdenv.isLinux;
      # let's try periodic optimization instead
      auto-optimise-store = false;
      experimental-features = ["nix-command" "flakes"];
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
