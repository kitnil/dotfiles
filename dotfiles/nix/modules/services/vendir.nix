{ config, lib, pkgs, home, ... }:

with lib;

let
  name = "vendir";
  cfg = config.services.vendir;
in
{
  options = {
    services.vendir = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Vendir.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.vendir;
        defaultText = "pkgs.vendir";
        description = ''
          Vendir.
        '';
      };
      vendirDirectory = mkOption {
        type = types.str;
        default = "${home.homeDirectory}/src";
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.vendir = {
      Unit = {
        Description = "Vendir";
      };
      Service = {
        WorkingDirectory = cfg.vendirDirectory;
        ExecStart = lib.strings.concatStringsSep " " [
          "${cfg.package}/bin/vendir"
          "sync"
        ];
        Type = "simple";
      };
    };
  };
}
