# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
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
    "bird/peers/nixos-workstation.conf" = {
      text = lib.readFile ./../peers/nixos-workstation.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-workstation.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
}
