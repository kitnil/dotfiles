{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.prometheus.exporters.custom.blackbox;
in
{
  options = {
    services.prometheus.exporters.custom.blackbox = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Blackbox.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    services.prometheus.exporters.blackbox = {
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
}
