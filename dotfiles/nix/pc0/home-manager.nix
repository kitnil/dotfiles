{ pkgs, packages, lib, ... }:

{
  home.username = "oleg";
  home.homeDirectory = "/home/oleg";
  manual.manpages.enable = false;

  home.packages = with packages; [
    nix
    viddy

    OVMF.fd # UEFI for virtual machines in libvirt

    robo3t

    chatterino2
  ];

  home.file = {
    ".mozilla/native-messaging-hosts/passff.json" = {
      text = builtins.toJSON {
        allowed_extensions = [ "passff@wugi.info" ];
        description = "Host for communicating with zx2c4 pass";
        name = "passff";
        path = "${pkgs.passff-host}/share/passff-host/passff.py";
        type = "stdio";
      };
    };
  };

  programs.firefox = {
    enable = true;
    profiles =
      let
        nix = {
          # TODO: Manage ~/.mozilla/firefox/nix/containers.json file with Nix.
          # TODO: Manage ~/.mozilla/firefox/nix/cookies.sqlite somehow.
          # TODO: Import ~/src/ssl/cert.p12 file with Nix.
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
        default = {
          # This profile not managed by Nix.
          name = "default";
          path = "pcaaxem9.default";
          isDefault = false;
          id = 0;
        };
        nix = nix // {
          name = "nix";
          id = 1;
          isDefault = true;
          extensions =
            with packages;
            with packages.nur.repos.rycee.firefox-addons;
            [
              copy-as-org-mode
              copy-all-tab-urls-we
              ublock-origin
            ];
        };
        twitch = {
          name = "twitch";
          id = 2;
          extensions =
            with packages;
            with packages.nur.repos.rycee.firefox-addons;
            [
              return-youtube-dislikes
              sponsorblock
              ublock-origin
              (betterttv.overrideAttrs (old: {
                version = "7.5.7";
                src = pkgs.fetchurl {
                  url = "https://addons.mozilla.org/firefox/downloads/file/4167416/betterttv-7.5.7.xpi";
                  sha256 = "ba9ed004c328f3dacb78537eceed9fc206d4e3a136bb80a1ed786dc9fb57b9d7";
                };
              }))
              metube-downloader
              night-video-tuner
              tab-reloader
              twitch-error-autorefresher
              visited-link-enabler
              ultrawidify
              web-scrobbler
            ];
          settings = {
            "browser.startup.homepage" = "about:addons";
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
