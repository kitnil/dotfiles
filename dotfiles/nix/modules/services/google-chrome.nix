{ config, lib, pkgs, ... }:

with lib;

let
  name = "google-chrome";
  cfg = config.services.google-chrome;
in
{
  options = {
    services.google-chrome = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Google Chrome.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.google-chrome;
        defaultText = "pkgs.google-chrome";
        description = ''
          Google Chrome.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.google-chrome = {
      Unit = {
        Description = "Google Chrome";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = pkgs.writeScript "google-chrome.sh" ''
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
