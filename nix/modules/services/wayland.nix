{ config, lib, pkgs, ... }:

with lib;

let
  name = "wayland";
  cfg = config.services.wayland;
in
{
  options = {
    services.wayland = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Wayland, the terminal.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.wayland = {
      Unit = {
        Description = "Wayland socket";
        StartLimitBurst = 5;
        StartLimitIntervalSec = 10;
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = pkgs.writeScript "wayland.sh" ''
          #!${pkgs.runtimeShell}

          ${pkgs.coreutils}/bin/ln -ns /mnt/guix/run/user/1000/wayland-1 /run/user/1000/
        '';
        Type = "oneshot";
        Restart = "never";
      };
    };
  };
}
