{ lib, config, inputs, ... }:

let
  cfg = config.virtualisation.kubevirt;
in
{
  options.virtualisation.kubevirt = {
    enable = lib.mkEnableOption "Enable kubevirt service";
  };
  config = lib.mkIf cfg.enable ({
    fileSystems = {
      "/var/run/kubevirt" = {
        device = "none";
        fsType = "tmpfs";
        options = [ "shared" ];
      };
    };
  });
}
