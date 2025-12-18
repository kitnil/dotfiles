{ config, lib, pkgs, ... }:

let
  inherit (pkgs) drbd9;
in {
  config = lib.mkIf ((config.services ? kubernetes-ha-master && config.services.kubernetes-ha-master.enable)
                     || (config.services ? kubernetes-ha-node && config.services.kubernetes-ha-node.enable)) ({
    boot.extraModulePackages = [
      drbd9
    ];
    services.udev.packages = [ drbd9 ];

    boot.kernelModules = [ "drbd" ];

    boot.extraModprobeConfig = ''
      options drbd usermode_helper=disabled
    '';

    services.lvm.boot.thin.enable = true; # Load kernel modules for thin LVM
  });
}
