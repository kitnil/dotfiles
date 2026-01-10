# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
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
    "bird/peers/guixsd.conf" = {
      text = lib.readFile ./../peers/guixsd.conf;
      mode = "0644";
    };
    "bird/peers/pc0.conf" = {
      text = lib.readFile ./../peers/pc0.conf;
      mode = "0644";
    };
    "bird/peers/nixos-gw.conf" = {
      text = lib.readFile ./../peers/nixos-gw.conf;
      mode = "0644";
    };
    "bird/peers/nixos-majordomo.conf" = {
      text = lib.readFile ./../peers/nixos-majordomo.conf;
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
    "bird/peers/nixos-ws.conf" = {
      text = lib.readFile ./../peers/nixos-ws.conf;
      mode = "0644";
    };
    "bird/peers/nixos-hev.conf" = {
      text = lib.readFile ./../peers/nixos-hev.conf;
      mode = "0644";
    };
  };
   systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/guixsd.conf".source
    config.environment.etc."bird/peers/pc0.conf".source
    config.environment.etc."bird/peers/nixos-gw.conf".source
    config.environment.etc."bird/peers/nixos-majordomo.conf".source
    config.environment.etc."bird/peers/nixos-tor.conf".source
    config.environment.etc."bird/peers/nixos-zapret.conf".source
    config.environment.etc."bird/peers/nixos-awg.conf".source
    config.environment.etc."bird/peers/nixos-ws.conf".source
    config.environment.etc."bird/peers/nixos-hev.conf".source
  ];
 systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  environment.systemPackages = [
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
