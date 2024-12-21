{ config, lib, pkgs, ... }:

with lib;

let
  name = "foot";
  cfg = config.services.foot;
in
{
  options = {
    services.foot = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Foot, the terminal.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.foot;
        defaultText = "pkgs.foot";
        description = ''
          Foot terminal.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.foot = {
      Unit = {
        Description = "Foot terminal";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = pkgs.writeScript "foot.sh" ''
          #!${pkgs.runtimeShell}

          XDG_RUNTIME_DIR=/mnt/guix/run/user/1000
          export XDG_RUNTIME_DIR

          WAYLAND_DISPLAY=wayland-1
          export WAYLAND_DISPLAY

          DISPLAY=:0
          export DISPLAY

          exec -a foot ${cfg.package}/bin/foot "$@"
        '';
        Type = "simple";
      };
    };
  };
}
