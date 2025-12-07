{
  description = "";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    dotfiles-home-manager.url = "git+file:/home/oleg/src/cgit.wugi.info/wigust/dotfiles?dir=nix";
    flake-utils.url = "github:numtide/flake-utils";
    firejail-disable-sandbox-check.url = "github:wigust/nixpkgs?ref=firejail-disable-sandbox-check";
  };

  outputs = { self
            , nixpkgs
            , flake-utils
            , dotfiles-home-manager
            , ... } @ inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      inherit (pkgs) mkShell nixStable nixos-install-tools;
    in
      rec {
        devShell.${system} = mkShell {
          buildInputs = [
            nixStable
            nixos-install-tools
          ];
          shellHook = ''
            . ${nixStable}/share/bash-completion/completions/nix
            export LANG=C
          '';
        };
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
            } // (if ech then {} else {
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
        nixosConfigurations = {
          nixos-systemd = nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = {
              inherit (inputs.dotfiles-home-manager.inputs) nixpkgs;
            };
            modules = [
              inputs.dotfiles-home-manager.inputs.home-manager.nixosModules.home-manager
              {
                home-manager = {
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  users = {
                    oleg = ./home-manager.nix;
                  };
                  sharedModules = [
                    inputs.dotfiles-home-manager.nixosModules.home-manager-firefox
                    inputs.dotfiles-home-manager.nixosModules.home-manager-foot
                    inputs.dotfiles-home-manager.nixosModules.home-manager-google-chrome
                    inputs.dotfiles-home-manager.nixosModules.home-manager-idea-community
                    inputs.dotfiles-home-manager.nixosModules.home-manager-pycharm-community
                    inputs.dotfiles-home-manager.nixosModules.home-manager-vendir
                    inputs.dotfiles-home-manager.nixosModules.home-manager-wayland
                  ];
                  extraSpecialArgs =
                    let
                      packages = with inputs.dotfiles-home-manager.inputs;
                        let
                          inherit (dotfiles-home-manager) overlay;
                        in
                          import inputs.dotfiles-home-manager.inputs.nixpkgs {
                            overlays = [
                              nur.overlays.default
                              flake-utils-plus.overlay
                              overlay
                            ];
                            inherit system;
                            config = {
                              allowUnfreePredicate = pkg:
                                builtins.elem (nixpkgs.lib.getName pkg) [
                                  "betterttv"
                                  "google-chrome"
                                ];
                              permittedInsecurePackages = [
                                "qtwebkit-5.212.0-alpha4"
                              ];
                            };
                          };
                    in
                      rec {
                        inherit (self) nixosConfigurations;
                        inherit inputs system;
                        pkgs = import nixpkgs {
                          inherit system;
                          config = {
                            allowUnfreePredicate = pkg:
                              builtins.elem (nixpkgs.lib.getName pkg) [
                                "google-chrome"
                                "IPMIView"
                                "ndi"
                                "steam"
                                "steam-unwrapped"
                                "teamspeak3"
                              ];
                          };
                        } // {
                          inherit (packages) google-chrome;
                          inherit (inputs.firejail-disable-sandbox-check.legacyPackages.${system})
                            firejail-disable-sandbox-check;
                        };
                        inherit packages customLib;
                      };
                };
              }
              ./hosts/nixos-systemd.nix
            ];
          };
        };
      };
}
