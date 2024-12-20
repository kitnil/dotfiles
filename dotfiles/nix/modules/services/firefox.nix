{ config, lib, pkgs, ... }:

with lib;

let
  name = "firefox";
  cfg = config.services.firefox;
in
{
  options = {
    services.firefox = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Firefox, the web browser.
        '';
      };
      package = mkOption {
        type = types.package;
        default = pkgs.firefox;
        defaultText = "pkgs.firefox";
        description = ''
          The Firefox package to run.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.firefox = {
      Unit = {
        Description = "Firefox web browser";
        After = [ "network.target" ];
        Environment = [
          "XDG_RUNTIME_DIR=/mnt/guix/run/user/1000"
          "WAYLAND_DISPLAY=wayland-1"
        ];
      };
      Service = {
        ExecStart = pkgs.writeScript "firefox.sh" ''
          #!${pkgs.runtimeShell}

          XDG_RUNTIME_DIR=/mnt/guix/run/user/1000
          export XDG_RUNTIME_DIR

          WAYLAND_DISPLAY=wayland-1
          export WAYLAND_DISPLAY

          exec -a firefox ${pkgs.firefox}/bin/firefox "$@"
        '';
        Type = "simple";
      };
    };
  };
}
