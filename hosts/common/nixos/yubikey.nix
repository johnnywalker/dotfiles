{pkgs, ...}: {
  services.udev.packages = [pkgs.yubikey-personalization];

  security.pam.u2f.cue = true;
  security.pam.services = {
    login.u2fAuth = true;
    sudo.u2fAuth = true;
  };
}
