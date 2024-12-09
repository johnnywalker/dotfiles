{
  pkgs,
  lib,
  ...
}: {
  busco = pkgs.stdenv.mkDerivation {
    name = "busco";
    version = "1.6";
    src = lib.cleanSourceWith {
      filter = path: type:
        builtins.elem (/. + path) [
          ./01058d09ae4547fd9651-1.6.xpi
        ];
      src = ./.;
    };
    phases = ["installPhase"];
    installPhase = ''
      cp $src/01058d09ae4547fd9651-1.6.xpi $out
    '';
  };
}
