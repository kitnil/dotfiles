{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.prometheus-lvm-exporter;
in
{
  options.services.prometheus-lvm-exporter = {
    enable = mkEnableOption "Enable Prometheus LVM exporter service";
  };
  config = mkIf cfg.enable {
    systemd.services.prometheus-lvm-exporter = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Environment = "PATH=${pkgs.lvm2.bin}/bin:$PATH";
        ExecStart = "${pkgs.prometheus-lvm-exporter}/bin/prometheus-lvm-exporter";
      };
    };
  };
}
