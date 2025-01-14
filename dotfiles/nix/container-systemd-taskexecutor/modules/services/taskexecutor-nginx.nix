{ config, lib, pkgs, majordomo-tls, ... }:

with lib;

let
  name = "taskexecutor-nginx";
  cfg = config.services.taskexecutor-nginx;
  tls = majordomo-tls;
in
{
  options = {
    services.taskexecutor-nginx = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable taskexecutor-nginx.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    system.activationScripts = {
      nginx-activate =
        let
          inherit (pkgs) coreutils;
        in
        ''
          ${coreutils}/bin/mkdir -p /opt/nginx/ssl
          ${coreutils}/bin/install -m400 ${tls.certificate} /opt/nginx/ssl/majordomo.ru.pem
          ${coreutils}/bin/install -m400 ${tls.key} /opt/nginx/ssl/majordomo.ru.key
        '';
    };

    fileSystems."/var/log/nginx" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "size=3G" "mode=755" ]; # mode=755 so only root can write to those files
    };
  };
}
