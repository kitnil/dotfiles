#!/usr/bin/env -S bash -c "nix-shell --run 'deploy . -- -L'"

{
  description = "Nix package manifest";
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    nixpkgs-20-03 = {
      url = "nixpkgs/nixos-20.03";
      flake = false;
    };
    nixpkgs-20-03-firefox = {
      url = "github:nixos/nixpkgs/cc3b6aa322f307580d48c975a3b86b4462b645d8";
      flake = false;
    };
    nixpkgs-phantomjs = {
      url = "github:NixOS/nixpkgs/ce9f1aaa39ee2a5b76a9c9580c859a74de65ead5";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nixpkgs-idea.url = "github:wigust/nixpkgs/a98b0d1e6d7bed029844576e8637ce9807600ad2";

    home-manager.url = "github:nix-community/home-manager?ref=release-21.05";
    nur.url = "github:nix-community/NUR";

    majordomo.url = "git+https://gitlab.intr/_ci/nixpkgs";

    github-com-guibou-nixGL = {
      url = "github:guibou/nixGL";
      flake = false;
    };
    github-com-kitnil-nix-docker-ipmi = {
      url = "github:kitnil/nix-docker-ipmi?ref=flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    github-com-kitnil-nix-ipmiview.url = "github:kitnil/nix-ipmiview";
    # github-com-xzfc-cached-nix-shell.url = "github:xzfc/cached-nix-shell";
    github-com-9999years-nix-config = {
      url = "github:9999years/nix-config";
      flake = false;
    };
    github-com-emilazy-mpv-notify-send = {
      url = "github:emilazy/mpv-notify-send";
      flake = false;
    };
    github-com-norfairking-dnscheck = {
      url = "github:kitnil/dnscheck/nix-pkgs-provide-system";
      flake = false;
    };
    github-com-tsoding-boomer = {
      url = "github:tsoding/boomer";
      flake = false;
    };

    deploy-rs.url = "github:serokell/deploy-rs";

    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nmattia/naersk";
    override.url = "nixpkgs";
    nixos.url = "nixpkgs/nixos-unstable";
    darwin.url = "github:LnL7/nix-darwin";
    bbuscarino-env.url = "github:wigust/env";
  };

  # nixConfig.allowUnfree = true;

  outputs = { self
            , nixpkgs
            , nixpkgs-20-03
            , nixpkgs-20-03-firefox
            , nixpkgs-phantomjs
            , deploy-rs
            , home-manager
            , nur
            , github-com-norfairking-dnscheck
            , github-com-guibou-nixGL
            , github-com-emilazy-mpv-notify-send
            , github-com-kitnil-nix-docker-ipmi
            , github-com-kitnil-nix-ipmiview
            , github-com-tsoding-boomer
            , majordomo
            , nixpkgs-idea
            , bbuscarino-env
            , ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      pkgs-20-03 = import nixpkgs-20-03 { inherit system; };
      pkgs-20-03-firefox = import nixpkgs-20-03-firefox {
        inherit system;
        config = {
          allowBroken = true;
          allowUnfree = true;
          allowUnsupportedSystem = true;
          firefox.icedtea = true;
          permittedInsecurePackages = [
            "autotrace-0.31.1"
            "batik-1.6"
            "firefox-52.9.0esr"
            "firefox-esr-unwrapped-52.9.0esr"
          ];
        };
      };
      lib = pkgs.lib;
    in {
      devShell.x86_64-linux = with pkgs; mkShell {
        buildInputs = [
          nixUnstable
          deploy-rs.outputs.packages.x86_64-linux.deploy-rs
        ];
      };
      packages.x86_64-linux =
        let
          jenkins-plugins = (import ./plugins.nix { inherit (pkgs) fetchurl stdenv; });
        in {
        # TODO: Flake inherit (pkgs.callPackage github-com-emilazy-mpv-notify-send) mpv-notify-send;

        inherit (import github-com-guibou-nixGL {
          inherit pkgs;
        }) nixGLIntel;

        inherit (pkgs-20-03)
          nixfmt
          robo3t;

        inherit (pkgs-20-03.python3Packages) yamllint;

        inherit (github-com-kitnil-nix-docker-ipmi.packages.${system}) ipmi;
        inherit (github-com-kitnil-nix-ipmiview.packages.${system}) ipmiview-wrapper;

        alerta = with pkgs-20-03; python3Packages.alerta.overrideAttrs (old: {
          patches = [
            (fetchurl {
              url = "https://raw.githubusercontent.com/kitnil/dotfiles/af9f1d52f78955f482d33b8c113c68728b0619f1/dotfiles/nix/patches/alerta-top-narrow-output.patch";
              sha256 = "07mwlm5x2ia5k05h8b8i53db0r4qpava1jlj6q96r0204qjmi897";
            })
          ];
        }
        );

        onefetch = with pkgs; onefetch.overrideAttrs (old: {
          patches = [
            (fetchurl {
              url = "https://github.com/o2sh/onefetch/commit/ae2cc1b35c876f8b092a1c7eb9fd9021354930a0.patch";
              sha256 = "0iizi5mbjwkbgy39nm9l9iw3l905zxd6v70n4x6zs5pxf9wwfzbx";
            })
          ];
        });

        # TODO: androidenv.androidPkgs_9_0.platform-tools

        firefox-52-wrapper = with pkgs-20-03-firefox; callPackage ({ stdenv, firefox-esr-52 }:
          stdenv.mkDerivation {
            inherit (firefox-esr-52) version;
            name = "firefox-esr-52";
            src = false;
            dontUnpack = true;
            buildInputs = [ firefox-esr-52 ];
            buildPhase = ''
              cat > firefox-esr-52 <<'EOF'
              #!${bash}/bin/bash -e
              exec -a firefox-esr-52 ${firefox-esr-52}/bin/firefox "$@"
              EOF
            '';
            installPhase = ''
              mkdir -p $out/bin
              install -m555 firefox-esr-52 $out/bin/firefox-esr-52
            '';
          }) {};

        jenkins = with pkgs;
          let
            pluginCmds = lib.attrsets.mapAttrsToList (n: v:
              "cp --recursive ${v}/*pi /home/oleg/.jenkins/plugins/${v.name}.jpi") jenkins-plugins;
          in callPackage ({ stdenv, lib, openjdk8 }:
            stdenv.mkDerivation {
              inherit (jenkins) version;
              name = "jenkins";
              src = false;
              dontUnpack = true;
              buildInputs = [ openjdk8 jenkins ];
              buildPhase = ''
                cat > jenkins <<'EOF'
                #!${bash}/bin/bash
                rm --force --recursive /home/oleg/.jenkins/{plugins,war}
                mkdir --parents /home/oleg/.jenkins/plugins
                ${lib.strings.concatStringsSep "\n" pluginCmds}
                exec -a "$0" ${openjdk8}/bin/java -Xmx512m -jar ${jenkins}/webapps/jenkins.war "$@"
                EOF
              '';
              installPhase = ''
                mkdir -p $out/bin
                install -m555 jenkins $out/bin/jenkins
              '';
            }) {};

        # TODO: Flake Add run-headphones.
        # operating-system = nixos { config = { services.headphones.enable = true; }; };
        # (stdenv.mkDerivation {
        #   name = "run-headphones";
        #   builder = writeScript "builder.sh" (''
        #     source $stdenv/setup
        #     mkdir -p $out/bin
        #     cat > $out/bin/run-headphones <<'EOF'
        #     #!${bash}/bin/bash
        #     exec -a headphones ${with lib; (head ((filterAttrs (n: v: n == "headphones") (foldAttrs (n: a: [ n ] ++ a) [ ] operating-system.options.systemd.services.definitions)).headphones)).serviceConfig.ExecStart} "$@"
        #     EOF
        #     chmod 555 $out/bin/run-headphones
        #   '');
        # });

        inherit (majordomo.packages.${system}) elktail;

        inherit (import github-com-norfairking-dnscheck) dnscheck;

        inherit ((import nixpkgs-idea { inherit system; config = { allowUnfree = true; }; }).idea)
          idea-ultimate pycharm-professional;

        } // ( {inherit (pkgs.nodePackages) node2nix; } )
        // (let boomer-repo = (github-com-tsoding-boomer.outPath + "/overlay"); in rec {
                  nim_1_0 = pkgs.callPackage (boomer-repo + "/nim_1_0.nix") {};
                  boomer = pkgs.callPackage (boomer-repo + "/boomer.nix") { inherit nim_1_0; };
                })
        // {
          inherit (import nixpkgs { inherit system; config = { allowUnfree = true; }; })
            discord google-chrome;
        }
        // {
          eve-online = pkgs.writeScriptBin "eve-online" ''
            #!${pkgs.runtimeShell}
            DRI_PRIME=1 ${self.packages.${system}.nixGLIntel}/bin/nixGLIntel ${bbuscarino-env.legacyPackages.${system}.eve-online}/bin/eve-online
          '';
        } // {
          onefetch = with pkgs;
            onefetch.overrideAttrs(old: {
              patches = [ (pkgs.fetchurl {
                url = "https://github.com/wigust/onefetch/commit/9c48548d8d1eaafa3e1776905f99a49bc1f2f462.patch";
                sha256 = "0sr7vs5z4k0bd6spgwnfxqg9d5479y9n5gznjf4nl165d9b87qrf";
              }) ];
            });
          jenkins-job-builder = pkgs.callPackage ({ stdenv, bash, jenkins-job-builder }:
            stdenv.mkDerivation {
              pname = "jenkins-job-builder";
              version = jenkins-job-builder.version;
              src = false;
              dontUnpack = true;
              buildInputs = [ bash jenkins-job-builder ];
              buildPhase = ''
                cat > jenkins-jobs <<'EOF'
                #!${bash}/bin/bash -e
                PYTHONPATH="" exec ${jenkins-job-builder}/bin/jenkins-jobs "$@"
                EOF
              '';
              installPhase = ''
                mkdir -p "$out"/bin
                install jenkins-jobs "$out"/bin/jenkins-jobs
              '';
            }) { inherit (pkgs-20-03.pythonPackages) jenkins-job-builder; };
          python-selenium =
            with import nixpkgs-phantomjs { inherit system; };
            let
              python3WithSelenium = python3.withPackages
                (python-packages: with python-packages; [ selenium ]);
            in writeScriptBin "python-selenium" ''
              #!${runtimeShell}
              PATH=${geckodriver}/bin:${firefox}/bin:"$PATH"
              export PATH
              exec -a "$0" ${python3WithSelenium}/bin/python "$@"
            '';
        };

      deploy.nodes.localhost =
        let
          dryActivateScript = pkgs.writeScript "deploy-rs-dry-activate" ''
            #!${pkgs.runtimeShell}
            echo $PROFILE
          '';
        in {
          hostname = "localhost";
          profiles = {
            # profile = {
            #   user = "oleg";
            #   autoRollback = false;
            #   magicRollback = false;
            #   path = (deploy-rs.lib.${system}.activate.custom // { dryActivate = dryActivateScript; })
            #     (pkgs.symlinkJoin {
            #       name = "profile";
            #       paths = with lib; collect isDerivation self.packages.${system};
            #     })
            #     ":";
            # };
            home-manager =
              let
                overlay = final: prev:
                  self.packages.${system} // {
                    inherit (deploy-rs.outputs.packages.${system}) deploy-rs;
                  };
                pkgs = import nixpkgs {
                  overlays = [ nur.overlay overlay ];
                  inherit system;
                };
              in rec
                {
                  user = "oleg";
                  profilePath = "/nix/var/nix/profiles/per-user/${user}/home-manager";
                  autoRollback = false;
                  magicRollback = false;
                  path = deploy-rs.lib.${system}.activate.home-manager (home-manager.lib.homeManagerConfiguration {
                    inherit pkgs system;
                    extraSpecialArgs = { inherit pkgs; };
                    homeDirectory = "/home/${user}";
                    username = user;
                    configuration = ./home-manager.nix;
                  });
                };
          };
        };

      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
