{config, ...}: {
  sops.secrets."yubikey/u2f_keys".path = "${config.xdg.configHome}/Yubico/u2f_keys";
}
