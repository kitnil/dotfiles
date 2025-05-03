{ pkgs, packages, lib, config, ... }:

let
  inherit (lib) fold;
in {
  home.username = "oleg";
  home.homeDirectory = "/home/oleg";
  manual.manpages.enable = false;

  home.packages = with packages; [
    viddy

    OVMF.fd # UEFI for virtual machines in libvirt

    nixGLIntel

    robo3t

    chatterino2

    tmux

    nekoray

    gnumake

    strace
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
    ".ssh/known_hosts" = {
      text = ''
        gitlab.corp1.majordomo.ru ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
        gitlab.corp1.majordomo.ru ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIpKca//ukVhXODbccv/mv4oG74h8jyNQmF7ZbWd/qaolkBv0ptb/ocPc47+btv+FQTx3Fj/cPyi83kwf3ow7C8=
        gitlab.intr ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJw9vd+rL+MwVdVKSKW32+k6irAULLUFv5dmRUve2nUW
      '';
    };
    ".gitconfig" = {
      text = ''
        [user]
          email = go.wigust@gmail.com
          name = Oleg Pykhalov
      '';
    };
  };

  programs.home-manager.enable = true;

  programs.firefox = {
    enable = true;
    profiles =
      let
        firefoxBaseProfile = {
          # TODO: Manage ~/.mozilla/firefox/nix/containers.json file with Nix.
          # TODO: Manage ~/.mozilla/firefox/nix/cookies.sqlite somehow.
          # TODO: Import ~/src/ssl/cert.p12 file with Nix.
          settings = {
            "browser.aboutConfig.showWarning" = false;
            "browser.search.region" = "GB";
            "browser.shell.checkDefaultBrowser" = false;
            "browser.startup.homepage" = "about:newtab";
            "browser.startup.page" = 3;
            "browser.urlbar.placeholderName" = "DuckDuckGo";
            "browser.urlbar.placeholderName.private" = "DuckDuckGo";
            "distribution.searchplugins.defaultLocale" = "en-GB";
            "doh-rollout.disable-heuristics" = true; # disable DNS over HTTPS
            "extensions.pocket.enabled" = false;
            "extensions.update.enabled" = false;
            "general.useragent.locale" = "en-GB";
            "general.warnOnAboutConfig" = false;
            "startup.homepage_welcome_url" = "about:newtab";
            "toolkit.telemetry.reportingpolicy.firstRun" = false;
          };
          search = {
            default = "DuckDuckGo";
          };
          extensions =
            with packages;
            with packages.nur.repos.rycee.firefox-addons;
            [
              certificate-pinner
              container-proxy
              copy-all-tab-urls-we
              copy-as-org-mode
              multi-account-containers
              snaplinksplus
              ublock-origin
            ];
        };
      in {
        default = {
          # This profile not managed by Nix.
          name = "default";
          path = "pcaaxem9.default";
          isDefault = false;
          id = 0;
        };
        nix = firefoxBaseProfile // {
          name = "nix";
          id = 1;
          isDefault = true;
          bookmarks = [
            {
              name = "home-karma";
              tags = [ "monitoring" "kubernetes" ];
              keyword = "karma";
              url = "https://karma.home.wugi.info";
            }
            {
              name = "home-grafana";
              tags = [ "monitoring" "kubernetes" ];
              keyword = "grafana";
              url = "https://grafana.home.wugi.info";
            }
            {
              name = "home-harbor";
              tags = [ "containers" "kubernetes" ];
              keyword = "karma";
              url = "https://harbor.home.wugi.info";
            }
            {
              name = "nix-homepage";
              tags = [ "nix" ];
              url = "https://nixos.org/";
            }
            {
              name = "nix-wiki";
              tags = [ "wiki" "nix" ];
              url = "https://wiki.nixos.org/";
            }
          ];
        };
        twitch = firefoxBaseProfile // {
          name = "twitch";
          id = 2;
          extensions =
            fold
              (extension: extensions: extensions ++ [extension])
              firefoxBaseProfile.extensions
              (with packages; with packages.nur.repos.rycee.firefox-addons; [
                return-youtube-dislikes
                sponsorblock
                hide-twitch-chat-users
                metube-downloader
                night-video-tuner
                soundfixer
                tab-reloader
                twitch-error-autorefresher
                visited-link-enabler
                ultrawidify
                web-scrobbler
              ]);
        };
        development = firefoxBaseProfile // {
          name = "development";
          id = 3;
          isDefault = false;
        };
        messaging = firefoxBaseProfile // {
          name = "messaging";
          isDefault = false;
          id = 4;
        };
        tor = firefoxBaseProfile // {
          name = "tor";
          id = 5;
          isDefault = false;
          settings = firefoxBaseProfile.settings // {
            "network.proxy.socks" = "example-tor-instance-tor-svc.tor-controller-instance";
            "network.proxy.socks_port" = 9050;
            "network.proxy.type" = 1;
          };
          bookmarks = [
            {
              name = "onion-search-duckduckgo";
              tags = [ "onion" "duckduckgo" ];
              keyword = "duckduckgo";
              url = "https://duckduckgogg42xjoc72x3sjasowoarfbgcmvfimaftt6twagswzczad.onion";
            }
            {
              name = "onion-search-brave";
              tags = [ "onion" "brave" ];
              keyword = "duckduckgo";
              url = "https://search.brave4u7jddbv7cyviptqjc7jusxh72uik7zt6adtckl5f4nwy2v72qd.onion";
            }
          ];
        };
        work = firefoxBaseProfile // {
          name = "work";
          id = 6;
          isDefault = false;
        };
      };
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host gitlab.intr
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass

      Host gitlab.corp1.majordomo.ru
      User git
      IdentityFile /home/oleg/.ssh/id_rsa_gitlab_intr_nopass
    '';
  };

  services.foot.enable = true;
  services.google-chrome.enable = true;
  services.pycharm-community.enable = true;
  services.idea-community.enable = true;
  services.vendir.enable = true;

  xresources.properties = {
    "Xcursor.theme" = "Adwaita";
    "Xcursor.size" = "48";
  };

  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "xterm-256color";
      };
      font = {
        size = 13;
      };
    };
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "23.05";
}
