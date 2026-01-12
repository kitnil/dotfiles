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
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-wan";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
}
