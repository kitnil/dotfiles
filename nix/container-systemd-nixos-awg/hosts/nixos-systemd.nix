# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  networking.firewall.enable = lib.mkForce true;
  programs.amnezia-vpn.enable = true;
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  services.resolved.enable = lib.mkForce false;
}
