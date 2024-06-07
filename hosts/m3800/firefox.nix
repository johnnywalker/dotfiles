{...}: {
  # Install Firefox
  programs.firefox.enable = true;
  programs.firefox.policies = {
    # Install Bitwarden
    ExtensionSettings."{446900e4-71c2-419f-a6a7-df9c091e268b}" = {
      install_url = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/latest.xpi";
      installation_mode = "force_installed";
      default_area = "navbar";
    };
    OfferToSaveLogins = false;
    PasswordManagerEnabled = false;

    # Install uBlock Origin
    ExtensionSettings."uBlock0@raymondhill.net" = {
      install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
      installation_mode = "force_installed";
      default_area = "navbar";
    };

    # Install Dark Reader
    ExtensionSettings."addon@darkreader.org" = {
      install_url = "https://addons.mozilla.org/firefox/downloads/latest/darkreader/latest.xpi";
      installation_mode = "force_installed";
      default_area = "navbar";
    };

    FirefoxHome = {
      Search = true;
      TopSites = false;
      SponsoredTopSites = false;
      Highlights = false;
      Pocket = true;
      SponsoredPocket = false;
      Snippets = false;
    };
    FirefoxSuggest = {
      WebSuggestions = false;
      SponsoredSuggestions = false;
      ImproveSuggest = false;
    };
  };
  programs.firefox.preferences = {
    "apz.gtk.pangesture.delta_mode" = 2;
    "apz.gtk.pangesture.pixel_delta_mode_multiplier" = "20.0";
    # disable swipe left/right
    "browser.gesture.swipe.left" = "";
    "browser.gesture.swipe.right" = "";
    "privacy.donottrackheader.enabled" = true;
    "privacy.globalprivacycontrol.enabled" = true;
    "privacy.globalprivacycontrol.functionality.enabled" = true;
  };
}
