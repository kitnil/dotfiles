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
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    github-com-guibou-nixGL = {
      url = "github:guibou/nixGL";
      flake = false;
    };
    # github-com-kitnil-nix-docker-ipmi.url = "github:kitnil/nix-docker-ipmi";
    # github-com-kitnil-nix-ipmiview.url = "github:kitnil/nix-ipmiview";
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
      url = "github:NorfairKing/dnscheck";
      flake = false;
    };

    deploy-rs.url = "github:serokell/deploy-rs";
  };

  # nixConfig.allowUnfree = true;

  outputs = { self, nixpkgs, nixpkgs-20-03, nixpkgs-20-03-firefox, deploy-rs, github-com-norfairking-dnscheck, github-com-guibou-nixGL, github-com-emilazy-mpv-notify-send, ... }:
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
      # inherit (import github-com-norfairking-dnscheck) dnscheck;
      my-packages = rec {
        inherit (pkgs)
          ansifilter
          bat
          bandwidth

          # alacritty
          # assh
          # browserpass
          # TODO: Flake boomer
          brave
          buku
          # cabal-install
          cached-nix-shell
          catimg
          chromium
          ctop
          diskus
          dive
          # dmg2img
          dnsperf
          # (import (fetchGit {
          #   url = "https://github.com/NorfairKing/dnscheck";
          #   ref = "master";
          # })).dnscheck
          # docker-compose
          docker-ls
          dragon-drop
          espanso
          # elktail-bin
          # ferm
          # filezilla
          # firefox
          fzf
          goldendict
          # geckodriver
          bfg-repo-cleaner
          git-secrets
          fac
          # glibc-locales
          go2nix
          glow
          groovy
          hexyl
          # httpie
          # mitmproxy # XXX: nix-env --set-flag priority
          hy
          hyperfine
          knot-resolver
          lexicon
          ldns
          # litecli TODO: fails to build
          navi
          lnav
          pastel
          procs
          # TODO: Flake hyperfine
          zenith
          tokei
          lua
          luarocks
          mediatomb
          mtr
          mycli
          dbeaver # SQL client
          mypaint
          nix
          nix-bash-completions
          nix-generate-from-cpan
          nix-prefetch-docker
          nix-serve
          noti

          logstalgia

          nixpkgs-lint
          # TODO: Fix steam ipmiview
          # nodePackages_12_x.node2nix

          gron
          pup # HTML parsing

          ioping

          audacity
          obs-studio
          scrcpy
          onefetch
          oh
          openjdk
          packer
          passff-host
          pgcli
          prettyping
          sampler
          screenkey
          skopeo
          # slack
          # slack-term
          tdesktop
          terraform
          thc-hydra
          thunderbird
          tldr
          ttyd
          ttyplot
          visidata
          webhook
          wrk
          wtf
          yq
          vmtouch

          adwaita-qt
          quassel

          zeal;

        # TODO: Flake inherit (pkgs.callPackage github-com-emilazy-mpv-notify-send) mpv-notify-send;

        inherit (pkgs.lxqt) qterminal;
        inherit (pkgs.libsForQt5) qtstyleplugins kde-gtk-config;

        inherit (pkgs.terraform-providers) docker github gitlab;

        inherit (pkgs.gitAndTools)
          delta
          git-extras
          git-open
          git-recent
          grv
          pre-commit;

        inherit (pkgs.haskellPackages) greenclip;

        # TODO: Flake inherit (import github-com-guibou-nixGL) nixGLIntel;

        inherit (pkgs-20-03)
          nixfmt
          robo3t;

        inherit (pkgs-20-03.pythonPackages) jenkins-job-builder;
        inherit (pkgs-20-03.python3Packages) yamllint;

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

        firefox-52 = pkgs-20-03-firefox.firefox-esr-52;
        firefox-52-wrapper = with pkgs-20-03-firefox; stdenv.mkDerivation {
          name = "firefox-esr-52";
          builder = writeScript "builder.sh" (''
            source $stdenv/setup
            mkdir -p $out/bin
            cat > $out/bin/firefox-esr-52 <<'EOF'
            #!${bash}/bin/bash -e
            exec -a firefox-esr-52 ${firefox-52}/bin/firefox "$@"
            EOF
            chmod 555 $out/bin/firefox-esr-52
          '');

          jenkins = with pkgs; stdenv.mkDerivation {
            name = "jenkins";
            builder = writeScript "builder.sh" (''
              source $stdenv/setup
              mkdir -p $out/bin
              cat > $out/bin/jenkins <<'EOF'
              #!${bash}/bin/bash
              exec -a "$0" ${openjdk8}/bin/java -Xmx512m -jar ${jenkins}/webapps/jenkins.war "$@"
              EOF
              chmod 555 $out/bin/jenkins
            '');
          };

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

        };
      };
    in {
      devShell.x86_64-linux = with pkgs; mkShell {
        buildInputs = [
          nixUnstable
          deploy-rs.outputs.packages.x86_64-linux.deploy-rs
        ];
      };
      packages.x86_64-linux = my-packages;
        #  // {
        #   union =
        #     let
        #       inherit (lib) collect isDerivation;
        #     in
        #       pkgs.callPackage ({ stdenv }:
        #         stdenv.mkDerivation rec {
        #           name = "packages-union";
        #           src = false;
        #           dontUnpack = true;
        #           buildInputs = lib.filter (package: isDerivation package)
        #             (collect isDerivation my-packages);
        #           buildPhase = false;
        #           installPhase = ''
        #           ${builtins.concatStringsSep "\n" (map (package: "echo ${package} >> $out") buildInputs)} 
        #         '';
        #         }) { };
        # }

      deploy.nodes.localhost = {
        hostname = "localhost";
        profiles.profile = {
          user = "oleg";
          path = deploy-rs.lib.${system}.activate.noop (pkgs.symlinkJoin {
            name = "profile";
            paths = with lib; collect isDerivation self.packages.x86_64-linux;
          });
        };
      };

      # TODO: Flake enable checks
      # checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
