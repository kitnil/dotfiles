{ config, lib, pkgs, ... }:

{
  config = {
    systemd.user.services.chatterino = {
      Unit = {
        Description = "Chatterino chat client for Twitch";
      };
      Service = {
        Environment = [
          "XDG_RUNTIME_DIR=/mnt/guix/run/user/%U"
          "WAYLAND_DISPLAY=wayland-1"
          "QT_QPA_PLATFORM=wayland"
        ];
        ExecStart = "${pkgs.chatterino2}/bin/chatterino";
        Type = "simple";
      };
    };
  };
}
