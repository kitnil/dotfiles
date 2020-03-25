with import <nixpkgs> {
  overlays = [
    (self: super: {
      nixGl = (import (builtins.fetchGit {
        url = "https://github.com/guibou/nixGL";
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
  (import (builtins.fetchGit {
    url = "file:///home/oleg/src/nixpkgs";
    rev = "04eb086e28f925a976b75d058f60d828beabcc69";
  }) {}).firefox
  fzf
  # geckodriver
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
  jenkins
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
  (mycli.overrideAttrs (oldAttrs: {
    propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [ python3Packages.paramiko ];
  }))
  mypaint
  nix
  nix-bash-completions
  nix-generate-from-cpan
  nix-prefetch-docker
  nix-serve

  old.nixfmt

  nixGl.nixGLIntel
  nixpkgs-lint
  ipmiview
  # nodePackages_12_x.node2nix

  # TODO: Fix catj
  # Setup: Encountered missing or private dependencies:
  # base >=4.12.0 && <4.13, megaparsec >=7.0.5 && <7.1
  old.catj

  obs-studio
  onefetch
  oh
  openjdk
  packer
  passff-host
  pgcli
  pup
  pythonPackages.jenkins-job-builder
  python3Packages.yamllint
  robo3t
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
]
