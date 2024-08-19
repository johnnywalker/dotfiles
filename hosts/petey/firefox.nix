{...}: {
  # Install Firefox
  programs.firefox.enable = true;
  programs.firefox.policies = {
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
    "privacy.donottrackheader.enabled" = true;
    "privacy.globalprivacycontrol.enabled" = true;
    "privacy.globalprivacycontrol.functionality.enabled" = true;
  };
}
