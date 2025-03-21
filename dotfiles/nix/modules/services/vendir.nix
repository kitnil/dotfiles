{ config, lib, pkgs, ... }:

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
        default = "${config.home.homeDirectory}/src";
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
        ExecStart = builtins.concatStringsSep " " [
          "${cfg.package}/bin/vendir"
          "sync"
        ];
        Environment = "PATH=${
          builtins.concatStringsSep ":" [
            "/run/wrappers/bin"
            "/run/current-system/sw/bin"
            "${config.home.homeDirectory}/.nix-profile/bin"
            "${pkgs.gitAndTools.git}/bin"
          ]
        }";
        Type = "simple";
      };
    };
  };
}
