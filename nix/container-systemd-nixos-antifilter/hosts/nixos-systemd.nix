# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
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
    "bird/peers/antifilter.0.conf" = {
      text = lib.readFile ./../peers/antifilter.0.conf;
      mode = "0644";
    };
    "bird/peers/nixos-gw.conf" = {
      text = lib.readFile ./../peers/nixos-gw.conf;
      mode = "0644";
    };
    "bird/peers/nixos-tor.conf" = {
      text = lib.readFile ./../peers/nixos-tor.conf;
      mode = "0644";
    };
    "bird/peers/nixos-zapret.conf" = {
      text = lib.readFile ./../peers/nixos-zapret.conf;
      mode = "0644";
    };
    "bird/peers/nixos-awg.conf" = {
      text = lib.readFile ./../peers/nixos-awg.conf;
      mode = "0644";
    };
    "bird/peers/nixos-hev.conf" = {
      text = lib.readFile ./../peers/nixos-hev.conf;
      mode = "0644";
    };
    "bird/peers/nixos-dante.conf" = {
      text = lib.readFile ./../peers/nixos-dante.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/antifilter.0.conf".source
    config.environment.etc."bird/peers/nixos-gw.conf".source
    config.environment.etc."bird/peers/nixos-tor.conf".source
    config.environment.etc."bird/peers/nixos-zapret.conf".source
    config.environment.etc."bird/peers/nixos-awg.conf".source
    config.environment.etc."bird/peers/nixos-hev.conf".source
    config.environment.etc."bird/peers/nixos-dante.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-antifilter";
  };
  services.dante = {
    enable = true;
    config = ''
      logoutput: syslog
      debug: 0

      external: eth0
      internal: eth0 port = 1080

      timeout.io: 60

      clientmethod: none
      socksmethod: none
      user.unprivileged: nobody

      client pass {
          from: 0.0.0.0/0 port 1-65535 to: 0.0.0.0/0
      }

      socks pass {
          from: 0.0.0.0/0 to: 0.0.0.0/0
          protocol: tcp udp
      }
    '';
  };
  services.mtr-exporter = {
    enable = true;
    address = "0.0.0.0";
    port = 31247;
    jobs = [
      {
        name = "vm1.wugi.info";
        address = "78.108.82.44";
        flags = [ "-n" ];
      }
    ];
  };
}
