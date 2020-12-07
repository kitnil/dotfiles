with import <nixpkgs> {
  overlays = [
    (self: super: {
      nixGl = (import (builtins.fetchGit {
        url = "https://github.com/guibou/nixGL";
        ref = "master";
      }) { });
      ipmi = (super.callPackage (builtins.fetchGit {
        url = "https://github.com/kitnil/nix-docker-ipmi";
        ref = "master";
      }) { });
      ipmiview = (super.callPackage (builtins.fetchGit {
        url = "https://github.com/kitnil/nix-ipmiview";
        ref = "master";
      }) { });
      cached-nix-shell = (super.callPackage (builtins.fetchGit {
        url = "https://github.com/xzfc/cached-nix-shell";
        ref = "master";
      }) { });
      navi = (super.callPackage ((builtins.fetchGit {
        url = "https://github.com/9999years/nix-config";
        ref = "main";
      }).outPath + "/rebeccapkgs/navi") { });
      mpv-notify-send = (super.callPackage ((builtins.fetchGit {
        url = "https://github.com/emilazy/mpv-notify-send";
        ref = "master";
      }).outPath + "/package.nix") { });
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
  }) { });

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
  chromium
  ctop
  diskus
  dive
  # dmg2img
  dnsperf
  (import (fetchGit {
    url = "https://github.com/NorfairKing/dnscheck";
    ref = "master";
  })).dnscheck
  # docker-compose
  docker-ls
  dragon-drop
  espanso
  # elktail-bin
  # ferm
  # filezilla
  firefox
  fzf
  goldendict
  # geckodriver
  bfg-repo-cleaner
  git-secrets
  # gita
  gitAndTools.delta
  gitAndTools.git-extras
  gitAndTools.git-open
  gitAndTools.git-recent
  gitAndTools.grv
  gitAndTools.pre-commit
  fac
  # glibc-locales
  go2nix
  glow
  haskellPackages.greenclip
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
  hyperfine
  zenith
  tokei
  lua
  luarocks
  mediatomb
  mpv-notify-send
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

  old.nixfmt

  nixGl.nixGLIntel
  nixpkgs-lint
  # TODO: Fix steam ipmiview
  # nodePackages_12_x.node2nix

  gron
  pup # HTML parsing

  ioping

  audacity
  obs-studio
  androidenv.androidPkgs_9_0.platform-tools
  scrcpy
  onefetch
  oh
  openjdk
  packer
  passff-host
  pgcli
  prettyping
  pup
  old.pythonPackages.jenkins-job-builder
  old.python3Packages.yamllint
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
  lxqt.qterminal

  (old.python3Packages.alerta.overrideAttrs (old: {
    patches = [
      /home/oleg/.local/share/chezmoi/dotfiles/nix/patches/alerta-top-narrow-output.patch
    ];
  }))

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
      exec -a "$0" ${openjdk8}/bin/java -Xmx512m -jar ${jenkins}/webapps/jenkins.war "$@"
      EOF
      chmod 555 $out/bin/jenkins
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

  zeal

]
