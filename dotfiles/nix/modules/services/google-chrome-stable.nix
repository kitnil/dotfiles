{ config, lib, pkgs, ... }:

with lib;

let
  name = "google-chrome-stable";
  cfg = config.services.google-chrome-stable;
in
{
  options = {
    services.google-chrome-stable = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Google Chrome.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.google-chrome-stable;
        defaultText = "pkgs.google-chrome-stable";
        description = ''
          Google Chrome.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.google-chrome-stable = {
      Unit = {
        Description = "Google Chrome";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = pkgs.writeScript "google-chrome-stable.sh" ''
          #!${pkgs.runtimeShell}

          XDG_RUNTIME_DIR=/mnt/guix/run/user/1000
          export XDG_RUNTIME_DIR

          WAYLAND_DISPLAY=wayland-1
          export WAYLAND_DISPLAY

          DISPLAY=:0
          export DISPLAY

          exec -a google-chrome-stable ${cfg.package}/bin/google-chrome-stable "$@"
        '';
        Type = "simple";
      };
    };
  };
}
