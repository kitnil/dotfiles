{
  description = "Dotfiles";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    home-manager = {
      url = "git+https://github.com/nix-community/home-manager?ref=release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nur }:
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
            ./modules/services/chatterino.nix
            ./modules/services/firefox.nix
            ./modules/services/foot.nix
            ./modules/services/google-chrome.nix
            ./modules/services/idea-community.nix
            ./modules/services/pycharm-community.nix
            ./modules/services/vendir.nix
            ./modules/services/wayland.nix
            ./container-systemd/home-manager.nix
          ];
          extraSpecialArgs = {
            inherit (self) nixosConfigurations;
            inherit customLib system;
            customModulesPath = ./modules;
          };
        in
        [
          {
            nixpkgs.overlays = [
              nur.overlays.default
            ];
            nixpkgs.config.allowUnfree = true;
            nixpkgs.system = system;
          }
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
                oleg = ./container-systemd/home-manager.nix;
              };
            };
          }
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
    in
    {
      nixosConfigurations = {
        container-systemd = nixpkgs.lib.nixosSystem {
          modules = commonModules;
        };
        container-systemd-nixos-workstation = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosWorkstationModules;
        };
        container-systemd-nixos-workstation-pc0 = nixpkgs.lib.nixosSystem {
          modules = containerSystemdNixosWorkstationPc0Modules;
        };
      };
    };
}
