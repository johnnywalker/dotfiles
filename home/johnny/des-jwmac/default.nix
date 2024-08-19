{...}: {
  imports = [
    ../common/presets/nix-darwin.nix
  ];

  programs.git = {
    userEmail = "jwalker@designsforhealth.com";
    signing = {
      key = "637096B053DC9185AA43EB7CE066C68A21EFECDE";
      signByDefault = true;
    };
  };

  home.file.".authinfo.gpg".source = ./authinfo.gpg;
}
