with import <nixpkgs> {
  overlays = [
    (self: super: {
      nixGl = (import (builtins.fetchGit {
        url = "https://github.com/guibou/nixGL";
        ref = "master";
      }) { });
      ipmi = (super.callPackage (builtins.fetchGit {
        url = "https://cgit.duckdns.org/git/nix/ipmi";
        ref = "master";
      }) { });
      ipmiview = (super.callPackage (builtins.fetchGit {
        url = "https://cgit.duckdns.org/git/nix/ipmiview";
        ref = "master";
      }) { });
      cached-nix-shell = (super.callPackage (builtins.fetchGit {
        url = "https://github.com/xzfc/cached-nix-shell";
        ref = "master";
      }) { });
      navi = (super.callPackage ((builtins.fetchGit {
        url = "https://github.com/9999years/nix-config";
        ref = "master";
      }).outPath + "/rebeccapkgs/navi") { });
    })
    (import ((builtins.fetchGit {
      url = "https://github.com/tsoding/boomer";
      ref = "master";
    }).outPath + "/overlay"))
  ];
  config = { allowUnfree = true; };
};

with pkgs;

let
  old = (import (fetchgit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "05626cc86b8a8bbadae7753d2e33661400ff67de";
    sha256 = "1lx5hs2yzg21pc8fv982kdqhc5j5kxi08h8wn3plx848bcnd8jj6";
  }) {
    overlays = [
      (self: super: {
        catj = (import (builtins.fetchGit {
          url = "https://cgit.duckdns.org/git/nix/catj";
          ref = "master";
        }) { });
      })
    ];
  });

  operating-system = nixos { config = { services.headphones.enable = true; }; };

in [
  # alacritty
  ansifilter
  # assh
  bat
  # browserpass
  bandwidth
  boomer
  brave
  buku
  # cabal-install
  cached-nix-shell
  catimg
  chezmoi
  chromium
  clipmenu
  ctop
  diskus
  dive
  # dmg2img
  dnsperf
  # docker-compose
  docker-ls
  dragon-drop
  # elktail-bin
  fd
  # ferm
  # filezilla
  firefox
  fzf
  # geckodriver
  bfg-repo-cleaner
  git-secrets
  # gita
  # glibc-locales
  go2nix
  glow
  groovy
  hexyl
  # httpie
  hy
  hyperfine
  knot-resolver
  ldns
  libressl
  # litecli TODO: fails to build
  navi
  lnav
  lsd
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

  old.nixfmt

  nixGl.nixGLIntel
  nixpkgs-lint
  # TODO: Fix steam ipmiview
  # nodePackages_12_x.node2nix

  # TODO: Fix catj
  # Setup: Encountered missing or private dependencies:
  # base >=4.12.0 && <4.13, megaparsec >=7.0.5 && <7.1
  old.catj

  obs-studio
  androidenv.androidPkgs_9_0.platform-tools
  scrcpy
  onefetch
  oh
  openjdk
  packer
  passff-host
  pgcli
  pup
  pythonPackages.jenkins-job-builder
  python3Packages.yamllint
  old.robo3t
  sampler
  screenkey
  skopeo
  # slack
  # slack-term
  tdesktop
  terraform
  terraform-providers.docker
  terraform-providers.github
  terraform-providers.gitlab
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

  adwaita-qt libsForQt5.qtstyleplugins kde-gtk-config
  quassel

  python3Packages.alerta

  (let
    firefox = (import (builtins.fetchTarball {
      url =
        "https://github.com/nixos/nixpkgs/archive/cc3b6aa322f307580d48c975a3b86b4462b645d8.tar.gz";
    }) {
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
    }).firefox-esr-52;

  in (stdenv.mkDerivation {
    name = "firefox-esr-52";
    builder = writeScript "builder.sh" (''
    source $stdenv/setup
    mkdir -p $out/bin
    cat > $out/bin/firefox-esr-52 <<'EOF'
    #!${bash}/bin/bash -e
    exec -a firefox-esr-52 ${firefox}/bin/firefox "$@"
    EOF
    chmod 555 $out/bin/firefox-esr-52
  '');
  }))

  (stdenv.mkDerivation {
    name = "jenkins";
    builder = writeScript "builder.sh" (''
      source $stdenv/setup
      mkdir -p $out/bin
      cat > $out/bin/jenkins <<'EOF'
      #!${bash}/bin/bash
      exec -a "$0" ${openjdk}/bin/java -Xmx512m -jar ${jenkins}/webapps/jenkins.war "$@"
      EOF
      chmod 555 $out/bin/jenkins
    '');
  })

  (stdenv.mkDerivation {
    name = "ipmi";
    builder = writeScript "builder.sh" (''
      source $stdenv/setup
      mkdir -p $out/bin

      cat > $out/bin/ipmi <<'EOF'
      #!${bash}/bin/bash
      # Wrapper which you could run as `ipmi jenkins.ipmi'
      export IPMI_HOST=$1
      export IPMI_OUTPUT=/tmp/$IPMI_HOST.jviewer.jnlp
      export IPMI_USER=ADMIN
      export IPMI_PASSWORD=$(pass show majordomo/ipmi/ADMIN)
      exec -a ipmi ${ipmi}/bin/ipmi "$@"
      EOF
      chmod 555 $out/bin/ipmi

      # Oracle's javaws configuration utility.
      ln -s ${ipmi}/bin/ControlPanel $out/bin/ControlPanel
    '');
  })

  (stdenv.mkDerivation {
    name = "run-headphones";
    builder = writeScript "builder.sh" (''
      source $stdenv/setup
      mkdir -p $out/bin
      cat > $out/bin/run-headphones <<'EOF'
      #!${bash}/bin/bash
      exec -a headphones ${with lib; (head ((filterAttrs (n: v: n == "headphones") (foldAttrs (n: a: [ n ] ++ a) [ ] operating-system.options.systemd.services.definitions)).headphones)).serviceConfig.ExecStart} "$@"
      EOF
      chmod 555 $out/bin/run-headphones
    '');
  })

]
