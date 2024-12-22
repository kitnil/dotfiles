{ config, lib, pkgs, ... }:

with lib;

let
  name = "wayvnc";
  cfg = config.services.wayvnc;
in
{
  options = {
    services.wayvnc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Wayvnc, the terminal.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.wayvnc;
        defaultText = "pkgs.wayvnc";
        description = ''
          Wayvnc terminal.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.wayvnc = {
      Unit = {
        Description = "Wayvnc terminal";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = pkgs.writeScript "wayvnc.sh" ''
          #!${pkgs.runtimeShell}

          XDG_RUNTIME_DIR=/mnt/guix/run/user/1000
          export XDG_RUNTIME_DIR

          WAYLAND_DISPLAY=wayland-1
          export WAYLAND_DISPLAY

          exec -a wayvnc ${cfg.package}/bin/wayvnc "$@"
        '';
        Type = "simple";
      };
    };
  };
}
