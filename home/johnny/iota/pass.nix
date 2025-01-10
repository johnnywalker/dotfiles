{
  # use pass for XDG secret service
  # - initialize store with `pass init <gpg-id>`
  programs.password-store.enable = true;
  services.pass-secret-service.enable = true;
}
