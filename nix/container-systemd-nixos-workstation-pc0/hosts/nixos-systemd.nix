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
      MIIDazCCAlOgAwIBAgIUaTln87emIUzpBnOwpRhKU5LD6y4wDQYJKoZIhvcNAQEL
      BQAwRTELMAkGA1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoM
      GEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZDAeFw0yNjAyMTQxMjA0NDFaFw0yNjAz
      MTYxMjA0NDFaMEUxCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEw
      HwYDVQQKDBhJbnRlcm5ldCBXaWRnaXRzIFB0eSBMdGQwggEiMA0GCSqGSIb3DQEB
      AQUAA4IBDwAwggEKAoIBAQC135HKq7lBGiPrHr8pS+REO2ww4wL9/+m4BBFloRd+
      g0FCDpVN+HaBEOyWFNkSsTEb+3mtAANXrqTDvcenrjYG3W9am+ZVDMnhsukiQ3SG
      zdq3nZN5t469jWdWlv7PFaHv2h6som8bXdmxPOXqJ3Z/99MOBJ6qfNYuZdnpOobM
      fhtt1cM3TcMKoecYgfYLejZA5mn5z5XHvBGla1GRl8weQqZ9wm6mvPBlJ4P1UwmE
      T5y8d3JMQfY0Gm2kcLVDNLM6GknvgvMhfMVdzscQMs8Vr2kiPbC3r0KTB/Wb+0R5
      k5dGhpEmdAd117jmLTdeJSipVSBUCiF2Jy7Bj8kK01HjAgMBAAGjUzBRMB0GA1Ud
      DgQWBBTCwyfRhHrpW9rn772udVvj7p7iSjAfBgNVHSMEGDAWgBTCwyfRhHrpW9rn
      772udVvj7p7iSjAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3DQEBCwUAA4IBAQCB
      Xq1o8A4SYXZAael5et8qZTNLx9P98gDmfqgAZpvH80gYsfYXR6G0yDfTfb/oZ8dW
      SP6OzyyKDGYB6WqP2sxvoHQGg3pKDQVKq2mujBeoYXRbEdz3m0Srms2NdjpBIvN3
      YC9R/e3slofPQZaNW6NzFL34J43hC73GEHjAdAbbtRNE9vR98EuSepQkPAj+aAEL
      BRbrfWpyywruB3IOJm3KBc2wFYfwd4S/m2MBXOAih4m79NGmDTuqB/6a+YGPQ6jZ
      +zOgOA6b/mzsgu7+8vapBJQBf56qTNRsTK64Xzldi21zL/NyZ9wQ8eRbI1G1KW7e
      xVtUidxhl7QZs6DeTV8U
      -----END CERTIFICATE-----
    ''
  ];
}
