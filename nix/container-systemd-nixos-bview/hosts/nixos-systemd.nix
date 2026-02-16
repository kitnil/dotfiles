# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

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
            neighbor-address = "192.168.0.180";
            peer-as = 64993;
          };
          apply-policy = {
            config = {
              default-import-policy = "reject-route";
              default-export-policy = "accept-route";
            };
          };
        }
      ];
    };
  };
  systemd.services.gobgpd = {
    serviceConfig = {
      ExecStartPost = "${pkgs.gobgp}/bin/gobgp mrt inject --no-ipv6 --only-best global /latest-bview";
    };
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-bview";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
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
  services.prometheus.exporters.bird = {
    enable = true;
  };
}
