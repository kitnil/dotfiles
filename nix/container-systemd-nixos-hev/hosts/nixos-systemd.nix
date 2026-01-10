# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  services.bird = {
    enable = true;
    config = lib.readFile ./../bird.1.conf;
    checkConfig = false;
  };
  environment.etc = {
    "bird/bird.conf" = {
      mode = "0644";
    };
    "bird/peers/nixos-antifilter.conf" = {
      text = lib.readFile ./../peers/nixos-antifilter.conf;
      mode = "0644";
    };
    "bird/peers/nixos-workstation.conf" = {
      text = lib.readFile ./../peers/nixos-workstation.conf;
      mode = "0644";
    };
    "bird/peers/nixos-dante.conf" = {
      text = lib.readFile ./../peers/nixos-dante.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-antifilter.conf".source
    config.environment.etc."bird/peers/nixos-dante.conf".source
    config.environment.etc."bird/peers/nixos-workstation.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  networking.firewall.enable = lib.mkForce true;
  services.prometheus.exporters.bird = {
    enable = true;
  };
  environment.systemPackages = [
    pkgs.ipset
    pkgs.lsof
    pkgs.strace
    pkgs.tcpdump
  ];
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-hev";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
  services.hev-socks5-tproxy = {
    enable = true;
    configFile = builtins.toFile "hev-socks5-tproxy.json" (builtins.toJSON {
      dns = { address = "::"; port = 1053; upstream = "192.168.0.192"; };
      main = { workers = 1; };
      socks5 = { address = "192.168.0.110"; port = 1080; udp = "udp"; };
      tcp = { address = "::"; port = 1088; };
      udp = { address = "::"; port = 1088; };
    });
  };
}
