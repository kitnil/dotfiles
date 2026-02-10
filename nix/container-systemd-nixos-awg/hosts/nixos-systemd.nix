# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  networking.firewall.allowedTCPPorts = [
    179
    31247
  ];
  programs.amnezia-vpn.enable = true;
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      adwaita-fonts
      dejavu_fonts
      wqy_zenhei
    ];
    fontconfig = {
      enable = true;
    };
  };
  services.resolved.enable = lib.mkForce false;
  systemd.services."AmneziaVPN".path = [
    pkgs.iptables
  ];

  programs.wireshark.enable = true;
  environment.systemPackages = [ pkgs.wireshark ];
  users.users.oleg.extraGroups = [ "wireshark" ];

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
    "bird/peers/nixos-dante.conf" = {
      text = lib.readFile ./../peers/nixos-dante.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-antifilter.conf".source
    config.environment.etc."bird/peers/nixos-dante.conf".source
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-awg";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
}
