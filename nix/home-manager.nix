{ pkgs, lib, ... }:

{
  home.username = "oleg";
  home.homeDirectory = "/home/oleg";
  manual.manpages.enable = false;

  programs.firefox = {
    enable = true;
    profiles =
      let
        nix = {
          # TODO: Manage ~/.mozilla/firefox/nix/containers.json file with Nix.
          # TODO: Manage ~/.mozilla/firefox/nix/cookies.sqlite somehow.
          # TODO: Import ~/src/ssl/cert.p12 file with Nix.
          extensions =
            with pkgs;
            with pkgs.nur.repos.rycee.firefox-addons;
            [
              auto_highlight
              auto-tab-discard
              clearurls
              container-proxy
              copy-all-tab-urls-we
              copy-link-text
              copy-selection-as-markdown
              cookie-quick-manager
              copy-as-org-mode
              darkreader
              foxscroller
              foxyproxy-standard
              gesturefy
              ghosttext
              google-container
              greasemonkey
              (firefox-addon-libredirect.overrideAttrs (old: {
                version = "2.1.0";
                src = pkgs.fetchurl {
                  url = "https://addons.mozilla.org/firefox/downloads/file/3960568/libredirect-2.1.0.xpi";
                  sha256 = "01zz4j85mlvsw41iwycw7zbyllx6q9j0i2l85sd47k0c8cf9jc14";
                };
              }))
              hello-goodbye
              (link-gopher.overrideAttrs (old: {
                version = "2.0.1";
                src = pkgs.fetchurl {
                  url = "https://addons.mozilla.org/firefox/downloads/file/3834730/link_gopher-2.0.1.xpi";
                  sha256 = "0kn5jl9nj6sp48ra0s75cla39w05rs40smvfsark3zdxankkmzry";
                };
              }))
              lovely-forks
              i-dont-care-about-cookies
              old-reddit-redirect
              metube-downloader
              new-window-without-toolbar
              redirector
              right-click-search
              rocker_gestures
              scroll_anywhere
              sitedelta-watch
              sponsorblock
              ublock-origin
              pkgs.access-control-allow-origin
              pkgs.snaplinksplus
              pkgs.prometheus-formatter
              single-file
              stylus
              tab-reloader
              tab-slideshow-we
              temporary-containers
              view-image
              visited-link-enabler
              ublacklist
              ultrawidify
            ];
          settings = {
            "browser.search.defaultenginename" = "Google";
            "browser.search.region" = "GB";
            "browser.shell.checkDefaultBrowser" = false;
            "browser.startup.homepage" = "about:newtab";
            "browser.startup.page" = 3;
            "distribution.searchplugins.defaultLocale" = "en-GB";
            "extensions.pocket.enabled" = false;
            "general.useragent.locale" = "en-GB";
            "general.warnOnAboutConfig" = false;
            "startup.homepage_welcome_url" = "about:newtab";
            "toolkit.telemetry.reportingpolicy.firstRun" = false;
          };
        };
      in {
        nix = nix // {
          name = "nix";
          id = 1;
          isDefault = true;
        };
        twitch = {
          name = "twitch";
          id = 2;
          extensions =
            with pkgs;
            with pkgs.nur.repos.rycee.firefox-addons;
            [
              return-youtube-dislikes
              sponsorblock
              ublock-origin
              metube-downloader
              night-video-tuner
              tab-reloader
              twitch-error-autorefresher
              visited-link-enabler
              ultrawidify
              web-scrobbler
              betterttv
            ];
          settings = {
            "browser.search.region" = "GB";
            "extensions.pocket.enabled" = false;
            "distribution.searchplugins.defaultLocale" = "en-GB";
            "general.useragent.locale" = "en-GB";
            "browser.search.defaultenginename" = "Google";
          };
        };
      };
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "23.05";
}
