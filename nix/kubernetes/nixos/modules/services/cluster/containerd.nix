{ lib, config, pkgs, inputs, ... }:

let
  inherit (inputs.ssl-certificates.lib) ssl;
  cfg = config.services.majordomo-containerd;
in

{
  options.services.majordomo-containerd = {
    bindAddress = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
    };
    bindPort = lib.mkOption {
      type = lib.types.int;
      default = 1338;
    };
  };
  config = lib.mkIf config.virtualisation.containerd.enable ({
    virtualisation = {
      containerd = {
        settings = {
          metrics = {
            address = cfg.bindAddress + ":" + (builtins.toString cfg.bindPort);
            # grpc_histogram = false;
          };
          plugins = {
            "io.containerd.grpc.v1.cri" = {
              # https://github.com/kubevirt/containerized-data-importer/issues/2378
              # Container disks imported to block volumes have incorrect
              # permissions · Issue #2378 · kubevirt/containerized-data-importer
              device_ownership_from_security_context = true;

              # https://github.com/containerd/containerd/blob/d1564fec5b06b43b46b089d2485bca19d84202be/docs/hosts.md#cri
              registry = {
                config_path = "/etc/containerd/certs.d";
              };
            };
          };
        };
      };
    };
    environment = {
      etc = {
        "containerd/certs.d/docker-registry.intr/hosts.toml" = {
          source = builtins.toFile "docker-registry.intr.toml" ''
            server = "https://docker-registry.intr"

            [host."https://docker-registry.intr"]
              capabilities = ["pull", "resolve"]
              ca = "${ssl."Majordomo_LLC_Root_CA.crt"}"
          '';
        };
      };
    };
    systemd = {
      services = {
        containerd = {
          preStart = ''
            export PATH=${pkgs.coreutils}/bin:$PATH
            mkdir -p /etc/containerd
            if [[ -e /etc/containerd/config.toml ]]
            then
                if diff ${config.virtualisation.containerd.args.config} /etc/containerd/config.toml
                then
                    install -Dm644 ${config.virtualisation.containerd.args.config} /etc/containerd/config.toml
                else
                    :
                fi
            else
                install -Dm644 ${config.virtualisation.containerd.args.config} /etc/containerd/config.toml
            fi
          '';
          serviceConfig = {
            ExecStart = lib.mkForce "${pkgs.containerd}/bin/containerd --config /etc/containerd/config.toml";
          };
          restartTriggers = [ config.environment.etc."containerd/certs.d/docker-registry.intr/hosts.toml".source ];
        };
      };
    };
  });
}
