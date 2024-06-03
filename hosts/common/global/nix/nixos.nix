{...}: {
  imports = [./shared.nix];
  nix = {
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 30d";
    };

    optimise = {
      automatic = true;
      dates = ["daily"];
    };
  };
}
