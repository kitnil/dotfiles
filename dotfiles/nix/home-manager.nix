{ pkgs, packages, lib, ... }:

{
  home.username = "oleg";
  home.homeDirectory = "/home/oleg";
  manual.manpages.enable = false;

  home.packages = with packages; [
    act

    ansifilter
    bat
    bandwidth
    bandwhich

    binwalk

    btfs

    clipman wayvnc wtype

    difftastic

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
    sumneko-lua-language-server # renamed to lua-language-server
    mycli
    mypaint
    nix-bash-completions
    nix-generate-from-cpan
    nix-prefetch-docker
    nix-serve
    noti
    clang-tools # for clangd in lsp-mode

    tmpmail

    tribler

    rnix-lsp
    yaml-language-server

    logstalgia

    nix
    nixos-install-tools
    nixos-rebuild
    nixpkgs-lint
    # nodePackages_12_x.node2nix
    fup-repl

    nodejs

    cachix

    mongodb

    gron
    pup # HTML parsing

    ioping
    iotop-c

    black

    nodePackages.vscode-json-languageserver-bin
    phpactor

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

    rust-analyzer

    bit

    yarn

    # TODO: Add xcolor after nixpkgs update.

    python-selenium

    zeal

    gitAndTools.delta
    gitAndTools.git-extras
    gitAndTools.git-open
    gitAndTools.git-recent
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
    elktail
    # eve-online
    firefox-52-wrapper
    mozilla-addons-to-nix
    fx_cast_bridge
    google-chrome
    chromium-wrapper # wraps google-chrome as chromium
    idea-ultimate
    ipmi
    ipmiview-wrapper
    ipmitool
    jenkins
    jenkins-job-builder
    # logstash

    jc

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

    moonlight-qt
  ];

  home.file = {
    ".bash.d/nix.bash" = {
      text = ''
        . ${packages.nix}/share/bash-completion/completions/nix

        . ${packages.vault-bin}/share/bash-completion/completions/vault
        complete -C ${packages.vault-bin}/bin/vault vault1
        complete -C ${packages.vault-bin}/bin/vault vault2
        complete -C ${packages.vault-bin}/bin/vault vault3
        complete -C ${packages.vault-bin}/bin/vault vault4
        complete -C ${packages.vault-bin}/bin/vault vault-ci
      '';
    };
    ".mozilla/native-messaging-hosts/passff.json" = {
      text = builtins.toJSON {
        allowed_extensions = [ "passff@wugi.info" ];
        description = "Host for communicating with zx2c4 pass";
        name = "passff";
        path = "${packages.passff-host}/share/passff-host/passff.py";
        type = "stdio";
      };
    };
  };

  programs.firefox = {
    enable = true;
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
        extensions = with packages.nur.repos.rycee.firefox-addons; [
          auto-tab-discard
          darkreader
          gesturefy
          ublock-origin
        ];
        settings = {
          "browser.startup.homepage" = "about:addons";
          "browser.search.region" = "GB";
          "distribution.searchplugins.defaultLocale" = "en-GB";
          "general.useragent.locale" = "en-GB";
          "browser.search.defaultenginename" = "Google";
        };
      };
      twitch = {
        name = "twitch";
        id = 2;
        extensions = with packages.nur.repos.rycee.firefox-addons; [
          ublock-origin
          packages."7tv"
          packages."twitch-points-autoclicker"
        ];
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

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "23.05";
}
