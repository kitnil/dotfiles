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
  boomer
  buku
  # cabal-install
  catimg
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
  firefox
  fzf
  # geckodriver
  git-secrets
  # gita
  # glibc-locales
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
  lnav
  lsd
  lua
  luarocks
  mediatomb
  mtr
  mycli
  mypaint
  nix
  nix-bash-completions
  nix-generate-from-cpan
  nix-prefetch-docker
  nix-serve
  nixfmt
  nixGl.nixGLIntel
  nixpkgs-lint
  # nodePackages_12_x.node2nix
  # node_catj
  obs-studio
  oh
  openjdk
  packer
  passff-host
  pgcli
  pup
  # python2.7-jenkins-job-builder
  # python3.7-yamllint
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
