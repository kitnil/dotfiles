# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  environment.systemPackages = [ pkgs.gobgp ];
  networking.firewall.allowedTCPPorts = [ 179 ];
  services.gobgpd = {
    enable = true;
    settings = {
      global = {
        config = {
          as = 64989;
          router-id = "192.168.0.150";
        };
      };
      neighbors = [
        {
          config = {
            neighbor-address = "192.168.0.195";
            peer-as = 64998;
          };
        }
      ];
    };
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-bview";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
}
