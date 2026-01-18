# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  environment.systemPackages = [
    pkgs.lsof
    pkgs.strace
    pkgs.tcpdump
    pkgs.openssl
  ];
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-kube103";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
  systemd.oomd.enable = false;
}
