{
  melpaBuild,
  lib,
  fetchFromGitHub,
}:
melpaBuild {
  pname = "eglot-booster";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "cab7803c4f0adc7fff9da6680f90110674bb7a22";
    sha256 = "sha256-xUBQrQpw+JZxcqT1fy/8C2tjKwa7sLFHXamBm45Fa4Y=";
  };

  meta = {
    description = "Boost eglot using lsp-booster";
    license = lib.licenses.gpl3;
    platforms = lib.platforms.all;
  };
}
