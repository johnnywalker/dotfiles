{
  inputs,
  pkgs,
  lib,
  ...
}: let
  pkgs-unstable = inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in {
  # Install Firefox
  programs.firefox.enable = true;
  # match mesa version if using hyprland flake
  # programs.firefox.package = pkgs-unstable.firefox;
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

    # Install search extension
    ExtensionSettings."{ed6f69b2-a3bd-40ae-925d-fbf9708ead61}" = {
      install_url = "file://" + (import ../common/global/firefox/extensions.nix {inherit pkgs lib;}).busco;
      installation_mode = "force_installed";
    };

    # Install Old Reddit Redirect
    ExtensionSettings."{9063c2e9-e07c-4c2c-9646-cfe7ca8d0498}" = {
      install_url = "https://addons.mozilla.org/firefox/downloads/latest/old-reddit-redirect/latest.xpi";
      installation_mode = "force_installed";
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
    # don't scroll forever
    "apz.fling_friction" = "0.005";
    "apz.gtk.pangesture.delta_mode" = 2;
    "apz.gtk.pangesture.pixel_delta_mode_multiplier" = "20.0";
    # disable swipe left/right
    "browser.gesture.swipe.left" = "";
    "browser.gesture.swipe.right" = "";
    "browser.tabs.insertAfterCurrent" = true;
    "privacy.donottrackheader.enabled" = true;
    "privacy.globalprivacycontrol.enabled" = true;
    "privacy.globalprivacycontrol.functionality.enabled" = true;
  };
}
