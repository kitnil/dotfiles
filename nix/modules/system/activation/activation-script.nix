{ config, lib, ... }:

with lib;

{
  options = {
    environment.sbinlvm = mkOption {
      default = null;
      type = types.nullOr types.path;
      description = ''
        Include a /bin/bash in the system.
      '';
    };
  };
  config = {
    system.activationScripts.sbinlvm = if config.environment.sbinlvm != null
      then ''
        mkdir -m 0755 -p /sbin
        ln -sfn ${config.environment.sbinlvm} /sbin/.lvm.tmp
        mv /sbin/.lvm.tmp /sbin/lvm # atomically replace /usr/sbin/env
      ''
      else ''
        rm -f /sbin/lvm
        rmdir -p /sbin || true
      '';
  };
}
