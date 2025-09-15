{
  melpaBuild,
  lib,
  fetchFromGitHub,
}:
melpaBuild {
  pname = "terraform-ts-mode";
  version = "0.6";

  src = fetchFromGitHub {
    owner = "kgrotel";
    repo = "terraform-ts-mode";
    rev = "985ed2a65dfdddcd50c5efd52975caff10ffb9d2";
    sha256 = "sha256-CzxH3pXPsMASCKpX6lxm2TNkl5GNKhtOJ34U+71wV8E=";
  };

  meta = {
    description = "Terraform major mode with treesit and eglot support";
    license = lib.licenses.gpl3;
    platforms = lib.platforms.all;
  };
}
