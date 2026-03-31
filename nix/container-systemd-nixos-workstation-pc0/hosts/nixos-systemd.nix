# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  programs.niri.enable = true;
  programs.sway.enable = true;
  programs.gamescope = {
    enable = true;
    capSysNice = false;
  };
  services.bird = {
    enable = true;
    config = lib.readFile ./../bird.1.conf;
    checkConfig = false;
  };
  environment.etc = {
    "bird/bird.conf" = {
      mode = "0644";
    };
    "bird/peers/nixos-hev.conf" = {
      text = lib.readFile ./../peers/nixos-hev.conf;
      mode = "0644";
    };
  };
   systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-hev.conf".source
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  environment.systemPackages = [
    pkgs.guile
    pkgs.mangohud
    pkgs.mtr
    pkgs.wireshark
  ];
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-workstation-pc0";
  };

  programs.wireshark.enable = true;
  users.users.oleg.extraGroups = [ "wireshark" ];
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };

  services.mediamtx = {
    enable = true;
    settings = {
      metrics = true;
      paths = {
        all = {
          source = "publisher";
        };
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    179                         # bgp (bird)
    9324                        # prometheus bird exporter
    11434                       # ollama
    31247                       # prometheus mtr exporters
  ];
  # https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    # Open ports in the firewall for Steam Remote Play
    remotePlay.openFirewall = true;
    # Open ports in the firewall for Source Dedicated Server
    dedicatedServer.openFirewall = true;
    # Open ports in the firewall for Steam Local Network Game Transfers
    localNetworkGameTransfers.openFirewall = true;
    # https://github.com/nixos/nixpkgs/issues/437281
    extraPackages = [
      pkgs.adwaita-icon-theme
      pkgs.gamescope
      pkgs.mangohud
      pkgs.xterm
    ];
    package = pkgs.steam.override {
      extraLibraries = pkgs: [ pkgs.xorg.libxcb ];
      extraEnv = {
        MANGOHUD = true;
        OBS_VKCAPTURE = true;
      };
      extraPkgs = pkgs: [
        pkgs.xorg.libXcursor
        pkgs.xorg.libXi
        pkgs.xorg.libXinerama
        pkgs.xorg.libXScrnSaver
        pkgs.libpng
        pkgs.libpulseaudio
        pkgs.libvorbis
        pkgs.stdenv.cc.cc.lib
        pkgs.libkrb5
        pkgs.keyutils
        pkgs.gamemode
      ];
    };
    extraCompatPackages = [
      pkgs.proton-ge-bin
    ];
  };
  systemd.services.reload-systemd-vconsole-setup.enable = false;

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
  };

  security.pki.certificates = [
    ''
      -----BEGIN CERTIFICATE-----
      MIIDrDCCApSgAwIBAgITeQ3TU2DMaFJUMo11dPW7JHKmKzANBgkqhkiG9w0BAQsF
      ADBmMQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwY
      SW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMR8wHQYDVQQDDBZ2ZG8uc29jaWFsc3Ry
      ZWFtLm5pbmphMB4XDTI2MDMxMzA5NDQ0MFoXDTI2MDQxMjA5NDQ0MFowZjELMAkG
      A1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoMGEludGVybmV0
      IFdpZGdpdHMgUHR5IEx0ZDEfMB0GA1UEAwwWdmRvLnNvY2lhbHN0cmVhbS5uaW5q
      YTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKsDV8yyUNVoyBrG3Igm
      jl1rDg/XlfL7iy0PBTRne6DD8Wr7TrhuuZksoNJK/67eJSsvsbN5X7200iii9P+C
      VYmdgTfQ5Xk0S2yzjLrBmWCzVfGviXerssgGWom6iEc+2yqZerEt+C9N71XUhkL4
      10F6e6/V2CmPB9qdTwdntigdQUahwhGjueogZ4kOYlz88eguOyg/IzK5P/22ERvI
      0NY/naBl1u9oj1rE5cB8HsVkGUx5BCU2yEssXYmmZ7tjP3AFwHbYOC21v+ECkeMR
      75bBUW2R82yzinUwoipSpqshzhUnGFnkxzH5fmA9n1mZWS6g8lk0IRzfdnl454Go
      7zsCAwEAAaNTMFEwHQYDVR0OBBYEFAYSjLgGPF42JNCLi01cfu+xUSfzMB8GA1Ud
      IwQYMBaAFAYSjLgGPF42JNCLi01cfu+xUSfzMA8GA1UdEwEB/wQFMAMBAf8wDQYJ
      KoZIhvcNAQELBQADggEBAAUiaO6FZpR16TVwXCntU5MYN6ThL15muCGWBY/5pLoq
      ydYIy1oSZMP9Kk9RAdQidhyQ1psh5UyDsh9I1ItrIEH6H94lS2pzVutUnrG67Ukh
      d5qE46s8Ol/eNOws/5WuriAnxIA0lWKeKejkpFb9WY4DhqKZgcvlag0OJ/vAcww/
      XdJdES1AF4ZIvh+ezIV6QRKh7HAavL0kWk3uIqISM3LLMJCo1MxLSWGPEw5OAYW5
      Bl8bQ9SFi8iJE6IzFQzwXtPSXh7UNe7NvQwTa5TFphInGuiMWocrfVq99gk2N6ln
      +h1UjaW9g1RvG3FZOo25zFlEX8NzuN0SF9Yh+724ps0=
      -----END CERTIFICATE-----
    ''
  ];
  services.ollama = {
    enable = true;
    package = pkgs.ollama-vulkan;
    host = "0.0.0.0";
  };

  console.enable = true;
  systemd.services."getty@tty1" = {
    enable = false;
  };
  systemd.services."autovt@tty1" = {
    enable = false;
  };
  systemd.services."getty@tty14" = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
  };
  services.getty.autologinUser = "oleg";
  services.logind = {
    settings.Login = {
      NAutoVTs = 0;
      ReserveVT = 0;
    };
  };
}
