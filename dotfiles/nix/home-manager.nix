{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    act

    ansifilter
    bat
    bandwidth
    bandwhich

    binwalk

    btfs

    clipman wayvnc wtype

    # alacritty

    # assh
    ssh-tools

    # browserpass
    brave
    buku
    # cabal-install
    cached-nix-shell
    catimg
    ctop
    diskus
    dive
    # dmg2img
    dnsperf
    # docker-compose
    docker-ls
    dogdns
    duf
    espanso
    # ferm
    filezilla
    # firefox
    adoptopenjdk-icedtea-web

    viddy

    fzf
    goldendict
    # geckodriver
    bfg-repo-cleaner
    git-secrets
    glab
    fac

    buildPackages.glibcLocales

    go2nix
    glow
    groovy
    hexyl
    httpie
    mitmproxy
    hy
    hyperfine
    knot-resolver
    # lexicon
    ldns
    litecli
    navi
    lnav
    pastel
    procs
    zenith
    tokei
    lua
    luarocks
    mycli
    mypaint
    nix-bash-completions
    nix-generate-from-cpan
    nix-prefetch-docker
    nix-serve
    noti
    clang-tools # for clangd in lsp-mode

    tribler

    rnix-lsp
    yaml-language-server

    logstalgia

    nixFlakes
    nixos-rebuild
    nixpkgs-lint
    # nodePackages_12_x.node2nix
    fup-repl

    cachix

    mongodb

    gron
    pup # HTML parsing

    ioping
    iotop-c

    black

    audacity
    scrcpy
    oh
    openjdk11
    packer
    passff-host
    pgcli
    prettyping
    sampler
    screenkey
    skopeo
    # slack
    # slack-term
    # tdesktop
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

    gping

    rls

    bit

    yarn

    # TODO: Add xcolor after nixpkgs update.

    python-selenium

    zeal

    gitAndTools.delta
    gitAndTools.git-extras
    gitAndTools.git-open
    gitAndTools.git-recent
    gitAndTools.grv
    gitAndTools.pre-commit

    libsForQt5.qtstyleplugins
    libsForQt5.kde-gtk-config

    lxqt.qterminal

    vault-bin
    vaultenv

    vagrant

    prometheus
    prometheus-pushgateway
    prometheus-alertmanager
    prometheus-dnsmasq-exporter
    prometheus-json-exporter

    OVMF.fd # UEFI for virtual machines in libvirt

    dhall
    dhall-nix
    dhall-json
    (haskell.lib.justStaticExecutables haskellPackages.dhall-yaml)

    json2hcl

    alerta
    # boomer
    deploy-rs
    discord
    dnscheck
    elktail
    # eve-online
    firefox-52-wrapper
    google-chrome
    idea-ultimate
    ipmi
    ipmiview-wrapper
    jenkins
    jenkins-job-builder

    # nim_1_0

    nixGLIntel
    nixfmt
    alejandra
    nixpkgs-fmt
    node2nix
    onefetch
    pycharm-professional
    robo3t
    yamllint

    restic-rest-server

    eiskaltdcpp

    sourcetrail
  ];

  home.file = {
    ".bash.d/nix.bash" = {
      text = ''
        . ${pkgs.nixFlakes}/share/bash-completion/completions/nix

        . ${pkgs.vault-bin}/share/bash-completion/completions/vault
        complete -C ${pkgs.vault-bin}/bin/vault vault1
        complete -C ${pkgs.vault-bin}/bin/vault vault2
        complete -C ${pkgs.vault-bin}/bin/vault vault3
        complete -C ${pkgs.vault-bin}/bin/vault vault4
        complete -C ${pkgs.vault-bin}/bin/vault vault-ci
      '';
    };
    ".mozilla/native-messaging-hosts/passff.json" = {
      text = builtins.toJSON {
        allowed_extensions = [ "passff@wugi.info" ];
        description = "Host for communicating with zx2c4 pass";
        name = "passff";
        path = "${pkgs.passff-host}/share/passff-host/passff.py";
        type = "stdio";
      };
    };
  };

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      gesturefy
      ublock-origin
    ];
    profiles = {
      default = {
        # This profile not managed by Nix.
        name = "default";
        path = "j56dvo43.default-1520714705340";
        isDefault = true;
        id = 0;
      };
      nix = {
        name = "nix";
        id = 1;
        settings = {
          "browser.startup.homepage" = "about:addons";
          "browser.search.region" = "GB";
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
          "browser.search.defaultenginename" = "Google";
        };
      };
    };
  };
}
