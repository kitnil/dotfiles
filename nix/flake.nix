{
  description = "Dotfiles";

  inputs = {
    home-manager = {
      url = "git+https://github.com/nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rycee-nur-expressions = {
      url = "git+https://gitlab.com/rycee/nur-expressions?dir=pkgs/firefox-addons";
    };
  };

  outputs = { self, nixpkgs, home-manager, nur, rycee-nur-expressions }:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      customLib = {
        firefoxBaseProfile = { ech ? false }: {
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
          } // (if ech then { } else {
            "network.dns.echconfig.enabled" = false;
            "network.dns.http3_echconfig.enabled" = false;
          });
          search = {
            force = true;
            default = "searxng";
            engines = {
              searxng = {
                name = "SearxNG";
                urls = [
                  {
                    template =
                      "https://searxng.home.wugi.info"
                      + "/search?q={searchTerms}&categories=general";
                  }
                ];
                definedAliases = [ "@sng" ];
              };
            };
          };
        };
      };
      commonModules =
        let
          sharedModules = [
            ./container-systemd/home-manager.nix
          ];
          extraSpecialArgs = {
            inherit (self) nixosConfigurations;
            inherit customLib system;
            customModulesPath = ./modules;
          };
        in
        [
          ({ pkgs, ... }:
            {
              nixpkgs.overlays = [
                nur.overlays.default
                self.overlay
              ];
              nixpkgs.config.allowUnfree = true;
              nixpkgs.system = system;
            })
          {
            # But NIX_PATH is still used by many useful tools, so we set it
            # to the same value as the one used by this flake.
            # Make `nix repl '<nixpkgs>'` use the same nixpkgs as the one
            # used by this flake.
            environment.etc."nix/inputs/nixpkgs".source = "${nixpkgs}";
            nix = {
              # Make `nix run nixpkgs#nixpkgs` use the same nixpkgs as the
              # one used by this flake.
              registry.nixpkgs.flake = nixpkgs;
              # Remove nix-channel related tools & configs, we use flakes
              # instead.
              channel.enable = false;
              settings = {
                # https://github.com/NixOS/nix/issues/9574
                nix-path = lib.mkForce "nixpkgs=/etc/nix/inputs/nixpkgs";
              };
            };
          }
          ./container-systemd/hosts/nixos-systemd.nix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              inherit sharedModules extraSpecialArgs;
              useGlobalPkgs = true;
              useUserPackages = true;
              users = {
                oleg = ./home-manager.nix;
              };
            };
          }
          {
            home-manager = {
              users = {
                oleg = ./container-systemd/home-manager.nix;
              };
            };
          }
          ./modules/services/webhook.nix
          ./modules/services/prometheus.nix
        ];
      containerSystemdNixosWorkstationModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-workstation/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-workstation/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosWorkstationPc0Modules = builtins.concatLists [
        containerSystemdNixosWorkstationModules
        [
          ./container-systemd-nixos-workstation-pc0/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-workstation-pc0/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosTorModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-tor/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-tor/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosAntifilterModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-antifilter/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-antifilter/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosGwModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-gw/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-gw/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosWanModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-wan/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-wan/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosZapretModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-zapret/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-zapret/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosBviewModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-bview/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-bview/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosAwgModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-awg/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-awg/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosWsModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-ws/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-ws/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosDanteModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-dante/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-dante/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
      containerSystemdNixosHevModules = builtins.concatLists [
        commonModules
        [
          ./container-systemd-nixos-hev/hosts/nixos-systemd.nix
          {
            home-manager = {
              users = {
                oleg = ./container-systemd-nixos-hev/oleg/home-manager.nix;
              };
            };
          }
        ]
      ];
    in
    {
      overlay = final: prev: {
        inherit (prev.callPackage ./firefox/generated-firefox-addons.nix {
          inherit (rycee-nur-expressions.lib.${system})
            buildFirefoxXpiAddon;
        })
          access-control-allow-origin
          auto_highlight
          certificate-pinner
          cookie-quick-manager
          copy-all-tab-urls-we
          copy-as-org-mode
          foxscroller
          google-container
          metube-downloader
          night-video-tuner
          hello-goodbye
          hide-twitch-chat-users
          prometheus-formatter
          right-click-search
          rocker_gestures
          russian-ru-language-pack
          scroll_anywhere
          sitedelta-watch
          snaplinksplus
          soundfixer
          tab-slideshow-we
          twitch-error-autorefresher
          view-page-archive
          visited-link-enabler
          ultrawidify;
        firefox-addon-libredirect =
          rycee-nur-expressions.packages.${system}.libredirect;
        socialstream = prev.callPackage ./pkgs/socialstream {};
        streamtitle = prev.callPackage ./pkgs/streamtitle {};
        yt-title-updater = prev.callPackage ./pkgs/yt-title-updater {};
        yt-title-updater-python = with prev;
          let
            python = python3.withPackages
              (python-packages: with python-packages; [
                urllib3
                google-api-python-client
                google-auth-oauthlib
                google-auth-httplib2
                pytz
                pyqt6
              ]);
          in writeScriptBin "python-yt-title-updater" ''
            #!${runtimeShell} -e
            exec ${python}/bin/python "$@"
          '';
      };
      nixosConfigurations = {
        container-systemd = nixpkgs.lib.nixosSystem {
          modules = commonModules;
        };
        container-systemd-nixos-workstation-pc0 = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosWorkstationPc0Modules;
        };
        container-systemd-nixos-tor = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosTorModules;
        };
        container-systemd-nixos-antifilter = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosAntifilterModules;
        };
        container-systemd-nixos-gw = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosGwModules;
        };
        container-systemd-nixos-wan = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosWanModules;
        };
        container-systemd-nixos-zapret = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosZapretModules;
        };
        container-systemd-nixos-bview = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosBviewModules;
        };
        container-systemd-nixos-awg = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosAwgModules;
        };
        container-systemd-nixos-ws = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosWsModules;
        };
        container-systemd-nixos-dante = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosDanteModules;
        };
        container-systemd-nixos-hev = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosHevModules;
        };
      };
    };
}
