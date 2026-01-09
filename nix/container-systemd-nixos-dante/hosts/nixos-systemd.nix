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
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-antifilter.conf".source
    config.environment.etc."bird/peers/nixos-workstation.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.tor = {
    enable = true;
    openFirewall = true;
    client = {
      enable = true;
      socksListenAddress = {
        addr = "0.0.0.0";
        port = 9050;
        IsolateDestAddr = true;
      };
    };
    settings.ControlPort = 9051;
  };
  networking.firewall.allowedTCPPorts = [
    1080
    9050                        # tor
  ];
  networking.firewall.enable = lib.mkForce true;
  services.prometheus.exporters.bird = {
    enable = true;
  };
  services.dante = {
    enable = true;
    config = lib.readFile ./../dante.conf;
  };
  environment.systemPackages = [
    pkgs.tcpdump
    pkgs.strace
  ];
}
