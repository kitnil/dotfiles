{ pkgs, lib, config, options, inputs, ... }:

with lib;
let
  cfg = config.services.kubernetes-ha-node;
  top = config.services.kubernetes;
in {
  imports = [ ./kubevirt.nix ../networking/cilium.nix ];
  options.services.kubernetes-ha-node = {
    enable = lib.mkEnableOption "Enable kubernetes service";
    bindAddress = mkOption {
      type = types.str;
      default = (builtins.head config.networking.interfaces.${
          builtins.head (lib.attrNames config.networking.interfaces)
        }.ipv4.addresses).address;
    };
    kubevirt = mkOption {
      default = false;
      type = types.bool;
    };
    # cluster = mkOption {
    #   type = types.listOf types.attrs;
    # };
    kubeApiServerAddress = mkOption {
      type = types.str;
    };
    kubeApiServerDnsName = mkOption {
      type = types.str;
    };
    certificates = mkOption {
      type = types.attrs;
      default = {};
    };
    hostName = mkOption {
      type = types.str;
      default = config.networking.hostName;
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.kubevirt {
      virtualisation.kubevirt = {
        enable = true;
      };
    })
    (lib.mkIf cfg.enable ({
      boot.kernel.sysctl = {
        "vm.max_map_count" = 262144;
        "net.ipv4.ip_nonlocal_bind" = 1; # for high-availability
        # The following configuration fixes error in
        # pod/opensearch-cluster-master-X:
        #
        # failed to create fsnotify watcher: too many open files.
        "fs.inotify.max_user_instances" = 256;
      };

      services = {
        flannel = {
          enable = false;
          network = config.services.kubernetes.clusterCidr;
          etcd = {
            certFile = cfg.certificates.kubernetes."flannel-client.pem";
            keyFile = cfg.certificates.kubernetes."flannel-client-key.pem";
            caFile = cfg.certificates.kubernetes."ca.pem";
            # endpoints =
            #   map
            #     (member: "https://${member.address}:2379")
            #     cfg.cluster;
          };

          storageBackend = lib.mkForce "etcd";
          # XXX: Should we configure flannel to use storageBackend "kubernetes"?
          # kubeconfig = top.lib.mkKubeConfig "flannel" {
          #   server = "https://${cfg.hostName}:6443";
          #   certFile = cfg.certificates.kubernetes."flannel-client.pem";
          #   keyFile = cfg.certificates.kubernetes."flannel-client-key.pem";
          # };
        };

        kubernetes = {
          masterAddress = cfg.kubeApiServerAddress;

          # Will be used in all 'kubeconfig' configurations.
          apiserverAddress = "https://${cfg.kubeApiServerDnsName}:6443";

          pki = { enable = false; };
          caFile = lib.mkForce cfg.certificates.kubernetes."ca.pem";

          kubeconfig = {
            certFile = cfg.certificates.kubernetes."kubernetes.pem";
            keyFile = cfg.certificates.kubernetes."kubernetes-key.pem";
          };

          # Use specific data directory for kubevirt, see:
          #
          # https://github.com/kubevirt/kubevirt/issues/5069
          # Failed to connect socket to '/var/run/libvirt/libvirt-sock ·
          # Issue #5069 · kubevirt/kubevirt
          dataDir = "/var/lib/kubelet";

          kubelet = {
            enable = true;
            # Generally, it is recommended to apply 0-4 as debug-level logs
            # and 5-8 as trace-level logs.
            #
            # Log verbosity Description
            #
            # --v=0 Always visible to an Operator.
            #
            # --v=1 A reasonable default log level if you do not want
            # --verbosity.
            #
            # --v=2 Useful steady state information about the service and
            # --important log messages that might correlate to significant
            # --changes in the system. This is the recommended default log
            # --level.
            #
            # --v=3 Extended information about changes.
            #
            # --v=4 Debug level verbosity.
            #
            # --v=6 Display requested resources.
            #
            # --v=7 Display HTTP request headers.
            #
            # --v=8 Display HTTP request contents.
            extraOpts = "--fail-swap-on=false --v=4";
            address = cfg.bindAddress;
            clientCaFile = cfg.certificates.kubernetes."ca.pem";
            tlsCertFile =
              cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}.pem";
            tlsKeyFile =
              cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}-key.pem";
            kubeconfig = {
              certFile =
                cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}.pem";
              keyFile =
                cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}-key.pem";
            };
          };

          proxy = {
            enable = false;
            bindAddress = cfg.bindAddress;
            kubeconfig = {
              certFile = cfg.certificates.kubernetes."kube-proxy.pem";
              keyFile = cfg.certificates.kubernetes."kube-proxy-key.pem";
            };
          };
        };

        nfs.server = {
          enable = true;
        };
      };

      # Required by third-party CNI installed outside of Nix.
      environment.etc = {
        "cni/net.d".enable = false;
        "cni/net.d/05-cilium.conf" = {
          text = builtins.toJSON config.services.kubernetes.cni.cilium.config;
          mode = "0644";
        };
      };

      systemd = {
        services = {
          containerd = {
            restartTriggers = [ config.environment.etc."cni/net.d".source ];
          };
          kubelet = {
            # Do not remove /opt/cni/bin directory context.
            preStart = lib.mkBefore ''
              rm() {
                return 0
              }
            '';
          };
        };
      };
    }))
  ];
}
