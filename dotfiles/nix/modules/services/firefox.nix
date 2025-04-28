{ config, lib, pkgs, ... }:

{
  config = {
    systemd.user.services."firefox@" = {
      Unit = {
        Description = "Firefox web browser";
      };
      Service = {
        ExecStart = ''
          #!${pkgs.runtimeShell}

          XDG_RUNTIME_DIR=/mnt/guix/run/user/%U
          export XDG_RUNTIME_DIR

          WAYLAND_DISPLAY=wayland-1
          export WAYLAND_DISPLAY

          exec -a firefox ${pkgs.firefox}/bin/firefox --profile %h/.mozilla/firefox/%i
        '';
        Type = "simple";
      };
    };
  };
}
