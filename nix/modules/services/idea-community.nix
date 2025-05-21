{ config, lib, pkgs, ... }:

with lib;

let
  name = "idea-community";
  cfg = config.services.idea-community;
in
{
  options = {
    services.idea-community = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Idea-Community.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.jetbrains.idea-community;
        defaultText = "pkgs.idea-community";
        description = ''
          Idea-Community.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.idea-community = {
      Unit = {
        Description = "Idea terminal";
      };
      Service = {
        ExecStart = pkgs.writeScript "idea-community.sh" ''
          #!${pkgs.runtimeShell}

          XDG_RUNTIME_DIR=/mnt/guix/run/user/1000
          export XDG_RUNTIME_DIR

          WAYLAND_DISPLAY=wayland-1
          export WAYLAND_DISPLAY

          DISPLAY=:0
          export DISPLAY

          _JAVA_AWT_WM_NONREPARENTING=1
          export _JAVA_AWT_WM_NONREPARENTING

          exec -a idea-community ${cfg.package}/bin/idea-community "$@"
        '';
        Type = "simple";
      };
    };
  };
}
