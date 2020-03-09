with import <nixpkgs> {
  overlays = [
    (self: super: {
      nixGl = (import (builtins.fetchGit {
        url = "https://github.com/guibou/nixGL";
        ref = "master";
      }) { });
      catj = (import (builtins.fetchGit {
        url = "https://cgit.duckdns.org/git/nix/catj";
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

[
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
  nixfmt
  nixGl.nixGLIntel
  nixpkgs-lint
  ipmiview
  # nodePackages_12_x.node2nix
  catj
  obs-studio
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
]
