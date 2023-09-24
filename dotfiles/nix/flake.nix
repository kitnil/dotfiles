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
    nixpkgs-phpactor.url = "nixpkgs/nixpkgs-unstable";

    nixpkgs-home-manager.url = "nixpkgs/nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager?ref=release-23.05";
    nur.url = "github:nix-community/NUR";
    rycee-nur-expressions.url = "git+https://gitlab.com/rycee/nur-expressions?dir=pkgs/firefox-addons";

    majordomo.url = "git+https://gitlab.intr/_ci/nixpkgs";

    majordomo-vault.url = "git+ssh://gitlab.intr/security/vault";

    github-com-guibou-nixGL = {
      url = "github:guibou/nixGL";
      flake = false;
    };
    github-com-kitnil-nix-docker-ipmi = {
      url = "github:kitnil/nix-docker-ipmi?ref=flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    github-com-kitnil-nix-ipmiview.url = "git+ssh://gitlab.intr/utils/ipmiview?ref=flake";
    # github-com-xzfc-cached-nix-shell.url = "github:xzfc/cached-nix-shell";
    github-com-9999years-nix-config = {
      url = "github:9999years/nix-config";
      flake = false;
    };
    github-com-emilazy-mpv-notify-send = {
      url = "github:emilazy/mpv-notify-send";
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
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";
    naersk.url = "github:nmattia/naersk";
    override.url = "nixpkgs";
    nixos.url = "nixpkgs/nixos-unstable";
    darwin.url = "github:LnL7/nix-darwin";
    bbuscarino-env.url = "github:wigust/env";

    kamadorueda-alejandra.url = "github:kamadorueda/alejandra/1.1.0";
  };

  # nixConfig.allowUnfree = true;

  outputs =
    { self
    , nixpkgs
    , nixpkgs-20-03
    , nixpkgs-20-03-firefox
    , nixpkgs-phantomjs
    , deploy-rs
    , nixpkgs-home-manager
    , home-manager
    , nur
    , rycee-nur-expressions
    , github-com-guibou-nixGL
    , github-com-emilazy-mpv-notify-send
    , github-com-kitnil-nix-docker-ipmi
    , github-com-kitnil-nix-ipmiview
    , github-com-tsoding-boomer
    , majordomo
    , majordomo-vault
    , nixpkgs-idea
    , nixpkgs-phpactor
    , bbuscarino-env
    , kamadorueda-alejandra
    , flake-utils-plus
    , ...
    }:
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
      inherit (lib)
        fold;
    in
    {
      devShell.${system} = with pkgs; mkShell {
        buildInputs = [
          nixUnstable
          deploy-rs.outputs.packages.${system}.deploy-rs
        ];
      };
      packages.${system} =
        let
          jenkins-plugins = (import ./plugins.nix { inherit (pkgs) fetchurl stdenv; });
        in
        fold
          (x: xs: xs // x)
          { }
          [
            {
              # TODO: Flake inherit (pkgs.callPackage github-com-emilazy-mpv-notify-send) mpv-notify-send;

              inherit (import github-com-guibou-nixGL {
                inherit pkgs;
              }) nixGLIntel;

              inherit (pkgs-20-03)
                nixfmt;

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

              # TODO: androidenv.androidPkgs_9_0.platform-tools

              firefox-52-wrapper = with pkgs-20-03-firefox;
                callPackage
                  ({ stdenv, firefox-esr-52 }:
                    writeScriptBin "firefox-esr-52" ''
                      #!${runtimeShell} -e
                      test_directory="$(mktemp -d)"
                      trap 'chmod -Rf +w "$test_directory"; rm -rf "$test_directory"' EXIT
                      exec -a "$0" ${firefox-esr-52}/bin/firefox --new-instance --profile "$test_directory" --private-window "$@"
                    '')
                  { };

              jenkins = with pkgs;
                let
                  pluginCmds = lib.attrsets.mapAttrsToList
                    (n: v:
                      "cp --recursive ${v}/*pi /home/oleg/.jenkins/plugins/${v.name}.jpi")
                    jenkins-plugins;
                in
                callPackage
                  ({ stdenv, lib, openjdk8 }:
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
                    })
                  { };

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

              inherit ((import nixpkgs-idea { inherit system; config = { allowUnfree = true; }; }).idea)
                idea-ultimate pycharm-professional;

              inherit (nixpkgs-phpactor.legacyPackages.${system})
                phpactor;
            }

            ({ inherit (pkgs.nodePackages) node2nix; })

            # XXX: Failed to build boomer.
            #
            # error: attribute 'lib' missing
            #
            #        at /nix/store/ny73wh68zm5gkgxz0pf7v2sf4cw3657x-source/overlay/boomer.nix:31:40:
            #
            #            30|   installPhase = "install -Dt $out/bin src/boomer";
            #            31|   fixupPhase = "patchelf --set-rpath ${stdenv.lib.makeLibraryPath [stdenv.cc.cc libX11 libXrandr libGL]} $out/bin/boomer";
            #              |                                        ^
            #            32| }
            #
            # // (let boomer-repo = (github-com-tsoding-boomer.outPath + "/overlay"); in rec {
            #           nim_1_0 = pkgs.callPackage (boomer-repo + "/nim_1_0.nix") {};
            #           boomer = pkgs.callPackage (boomer-repo + "/boomer.nix") { inherit nim_1_0; };
            #         })
            (
              let
                pkgs = import nixpkgs {
                  inherit system;
                  config = { allowUnfree = true; };
                };
              in
              {
                inherit (pkgs) discord google-chrome;
                chromium-wrapper = with pkgs;
                  callPackage
                    ({ stdenv, google-chrome }:
                      stdenv.mkDerivation {
                        name = "chromium";
                        src = false;
                        dontUnpack = true;
                        installPhase = ''
                          mkdir -p $out/bin
                          ln -s ${google-chrome}/bin/google-chrome-stable $out/bin/chromium
                        '';
                      })
                    { };
              }
            )

            {
              eve-online = pkgs.writeScriptBin "eve-online" ''
                #!${pkgs.runtimeShell}
                DRI_PRIME=1 ${self.packages.${system}.nixGLIntel}/bin/nixGLIntel ${bbuscarino-env.legacyPackages.${system}.eve-online}/bin/eve-online
              '';
            }
            {
              jenkins-job-builder = pkgs.callPackage
                ({ stdenv, bash, jenkins-job-builder }:
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
                  })
                { inherit (pkgs-20-03.pythonPackages) jenkins-job-builder; };
              python-selenium =
                with import nixpkgs-phantomjs { inherit system; };
                let
                  python3WithSelenium = python3.withPackages
                    (python-packages: with python-packages; [
                      selenium
                      prometheus_client
                      pyaml
                    ]);
                in
                writeScriptBin "python-selenium" ''
                  #!${runtimeShell}
                  PATH=${geckodriver}/bin:${firefox}/bin:"$PATH"
                  export PATH
                  exec -a "$0" ${python3WithSelenium}/bin/python "$@"
                '';
            }
          ];

      deploy.nodes.localhost =
        let
          dryActivateScript = pkgs.writeScript "deploy-rs-dry-activate" ''
            #!${pkgs.runtimeShell}
            echo $PROFILE
          '';
        in
        {
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
                  self.packages.${system} // (
                    let
                      inherit (rycee-nur-expressions.lib.${system}) buildFirefoxXpiAddon;
                      inherit (prev) callPackage;
                    in {
                      inherit (deploy-rs.outputs.packages.${system}) deploy-rs;
                      inherit (majordomo-vault.inputs.nixpkgs.legacyPackages.${system}) vault-bin;
                      inherit (import (rycee-nur-expressions.outPath + "/default.nix") { pkgs = prev; })
                        mozilla-addons-to-nix;
                      inherit (callPackage ./firefox/generated-firefox-addons.nix { inherit buildFirefoxXpiAddon; })
                        access-control-allow-origin
                        cookie-quick-manager
                        copy-all-tab-urls-we
                        foxscroller
                        highlightall
                        metube-downloader
                        hello-goodbye
                        prometheus-formatter
                        right-click-search
                        scroll_anywhere
                        snaplinksplus
                        tab-slideshow-we
                        view-page-archive
                        visited-link-enabler
                        ultrawidify;
                      inherit (rycee-nur-expressions.packages.${system})
                        container-proxy
                        copy-link-text
                        copy-selection-as-markdown
                        forget_me_not
                        link-gopher
                        lovely-forks
                        passff
                        redirector
                        single-file
                        tab-reloader;
                      alejandra = (kamadorueda-alejandra.packages.${system}).alejandra-x86_64-unknown-linux-gnu;
                      viddy = prev.viddy.overrideAttrs (old: {
                        patches = [
                          ./patches/viddy-add-maxhistory-argument.patch
                        ];
                      });
                    }
                  );
                pkgs = import nixpkgs {
                  overlays = [
                    nur.overlay
                    flake-utils-plus.overlay
                    overlay
                  ];
                  inherit system;
                  config = {
                    allowUnfreePredicate = pkg:
                      builtins.elem (lib.getName pkg) [
                        "betterttv"
                      ];
                    permittedInsecurePackages = [
                      "qtwebkit-5.212.0-alpha4"
                    ];
                  };
                };
              in
              rec
              {
                user = "oleg";
                profilePath = "/nix/var/nix/profiles/per-user/${user}/home-manager";
                autoRollback = false;
                magicRollback = false;
                path = deploy-rs.lib.${system}.activate.home-manager (home-manager.lib.homeManagerConfiguration {
                  pkgs = nixpkgs-home-manager.legacyPackages.${system};
                  extraSpecialArgs = {
                    packages = pkgs;
                  };
                  modules = [
                    ./home-manager.nix
                  ];
                });
              };
          };
        };

      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
