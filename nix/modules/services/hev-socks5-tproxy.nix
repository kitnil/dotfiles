{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.hev-socks5-tproxy;
in
{
  options = {
    services.hev-socks5-tproxy = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Blackbox.
        '';
      };

      package = mkOption {
        type = types.package;
        default = pkgs.hev-socks5-tproxy;
        internal = true;
        description = ''
          The hev-socks5-tproxy package.
        '';
      };

      configFile = mkOption {
        type = types.path;
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.services.hev-socks5-tproxy = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/hev-socks5-tproxy ${cfg.configFile}";
      };
    };
  };
}
