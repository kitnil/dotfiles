{ config, lib, inputs, system, ... }:

with lib;

let
  cfg = config.services.majordomo-prometheus;
  process_exporter =
    inputs.nix-flake-common.packages."${system}".process_exporter;
in
{
  options.services.majordomo-prometheus = {
    enable = lib.mkEnableOption "Enable Prometheus service";
    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = with lib;
        let
          interfacesVlan253 =
            filterAttrs
              (name: _:
                builtins.match "(([[:alpha:]]\|-)*253+.*)" name != null
                # TODO: Match ipv4 addresses instead of interface, because
                # could be a security issue in case enp0s4 has a public IP
                # address.
                || elem name [ "enp0s4" ])
              config.networking.interfaces;
        in
          (head (head (attrValues interfacesVlan253)).ipv4.addresses).address;
    };
  };
  config = lib.mkIf cfg.enable ({
    services.prometheus = {
      enable = false;
      exporters = {
        node = {
          enable = true;
          inherit (cfg) listenAddress;
          enabledCollectors = [ "systemd" "logind" "processes" ];
        };
      };
    };
  });
}
