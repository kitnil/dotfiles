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
    # "bird/peers/antifilter.1.conf" = {
    #   text = lib.readFile ./../peers/antifilter.1.conf;
    #   mode = "0644";
    # };
    "bird/peers/nixos-gw.conf" = {
      text = lib.readFile ./../peers/nixos-gw.conf;
      mode = "0644";
    };
    "bird/peers/nixos-wan.conf" = {
      text = lib.readFile ./../peers/nixos-wan.conf;
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
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/antifilter.0.conf".source
    config.environment.etc."bird/peers/antifilter.1.conf".source
    config.environment.etc."bird/peers/nixos-gw.conf".source
    config.environment.etc."bird/peers/nixos-wan.conf".source
    config.environment.etc."bird/peers/nixos-tor.conf".source
    config.environment.etc."bird/peers/nixos-zapret.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
}
