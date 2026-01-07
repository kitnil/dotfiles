# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, ... }:

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
    "bird/peers/nixos-tor.conf" = {
      text = lib.readFile ./../peers/nixos-tor.conf;
      mode = "0644";
    };
    "bird/peers/nixos-zapret.conf" = {
      text = lib.readFile ./../peers/nixos-zapret.conf;
      mode = "0644";
    };
  };
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
}
