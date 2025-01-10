{ config, lib, pkgs, ... }:

with lib;

let
  name = "pycharm-community";
  cfg = config.services.pycharm-community;
in
{
  options = {
    services.pycharm-community = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Pycharm-Community.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.pycharm-community;
        defaultText = "pkgs.pycharm-community";
        description = ''
          Pycharm-Community.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.pycharm-community = {
      Unit = {
        Description = "Pycharm terminal";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = pkgs.writeScript "pycharm-community.sh" ''
          #!${pkgs.runtimeShell}

          XDG_RUNTIME_DIR=/mnt/guix/run/user/1000
          export XDG_RUNTIME_DIR

          WAYLAND_DISPLAY=wayland-1
          export WAYLAND_DISPLAY

          DISPLAY=:0
          export DISPLAY

          _JAVA_AWT_WM_NONREPARENTING=1
          export _JAVA_AWT_WM_NONREPARENTING

          exec -a pycharm-community ${cfg.package}/bin/pycharm-community "$@"
        '';
        Type = "simple";
      };
    };
  };
}
