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
    "bird/peers/nixos-wan.conf" = {
      text = lib.readFile ./../peers/nixos-wan.conf;
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
    "bird/peers/nixos-awg.conf" = {
      text = lib.readFile ./../peers/nixos-awg.conf;
      mode = "0644";
    };
    "bird/peers/nixos-hev.conf" = {
      text = lib.readFile ./../peers/nixos-hev.conf;
      mode = "0644";
    };
    "bird/peers/nixos-dante.conf" = {
      text = lib.readFile ./../peers/nixos-dante.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/antifilter.0.conf".source
    # config.environment.etc."bird/peers/antifilter.1.conf".source
    config.environment.etc."bird/peers/nixos-gw.conf".source
    config.environment.etc."bird/peers/nixos-wan.conf".source
    config.environment.etc."bird/peers/nixos-tor.conf".source
    config.environment.etc."bird/peers/nixos-zapret.conf".source
    config.environment.etc."bird/peers/nixos-awg.conf".source
    config.environment.etc."bird/peers/nixos-hev.conf".source
    config.environment.etc."bird/peers/nixos-dante.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.prometheus.exporters = {
    bird = {
      enable = true;
    };
    blackbox = {
      enable = true;
      configFile = builtins.toFile "blackbox.json" (builtins.toJSON {
        modules = {
          http_2xx = {
            http = {
              follow_redirects = true;
              preferred_ip_protocol = "ip4";
              valid_http_versions = [ "HTTP/1.1" "HTTP/2.0" ];
            };
            prober = "http";
            timeout = "5s";
          };
          support_task_fix_wordpress = {
            http = {
              fail_if_body_not_matches_regexp = [ ".*Поздравляем.*" ];
              follow_redirects = false;
              preferred_ip_protocol = "ip4";
              valid_http_versions = [ "HTTP/1.1" "HTTP/2.0" ];
            };
            prober = "http";
            timeout = "5s";
          };
        };
      });
    };
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-antifilter";
  };
}
