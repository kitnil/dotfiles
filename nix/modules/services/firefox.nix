{ config, lib, pkgs, ... }:

{
  config = {
    systemd.user.services."firefox@" = {
      Unit = {
        Description = "Firefox web browser";
      };
      Service = {
        Environment = [
          "XDG_RUNTIME_DIR=/mnt/guix/run/user/%U"
          "WAYLAND_DISPLAY=wayland-1"
        ];
        ExecStart = "${pkgs.firefox}/bin/firefox --profile %h/.mozilla/firefox/%i --name firefox-%i";
        Type = "simple";
      };
    };
  };
}
