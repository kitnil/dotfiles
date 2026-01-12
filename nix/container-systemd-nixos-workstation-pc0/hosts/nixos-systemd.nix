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
    "bird/peers/nixos-hev.conf" = {
      text = lib.readFile ./../peers/nixos-hev.conf;
      mode = "0644";
    };
  };
   systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
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
}
