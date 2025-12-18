{ pkgs, lib, config, options, inputs, ... }:

with lib;
let
  cfg = config.services.kubernetes-ha-master;
  top = config.services.kubernetes;
  stringOrFile = cert:
    if builtins.isString cert then cert else readFile cert;
in
{
  imports = [ ../networking/cilium.nix ];
  options.services.kubernetes-ha-master = {
    enable = lib.mkEnableOption "Enable kubernetes service";
    bindAddress = mkOption {
      type = types.str;
      default = (builtins.head config.networking.interfaces.${
      builtins.head (lib.attrNames config.networking.interfaces)
      }.ipv4.addresses).address;
    };
    bindInterface = mkOption {
      type = types.str;
      default = "enp0s4";
    };
    cluster = mkOption {
      type = types.listOf types.attrs;
    };
    kubeApiServerAddress = mkOption {
      type = types.str;
    };
    kubeApiServerDnsName = mkOption {
      type = types.str;
    };
    certificates = mkOption {
      type = types.attrs;
      default = { };
    };
    hostName = mkOption {
      type = types.str;
      default = config.networking.hostName;
    };
    virtualRouterId = mkOption {
      type = types.int;
      default = 80;
    };
    replicas = mkOption {
      type = types.int;
      default = 3;
    };
  };
  config = lib.mkIf cfg.enable
    (mkMerge
      (fold (config: configs: configs ++ config)
        [
          {
            boot.kernel.sysctl = {
              "vm.max_map_count" = 262144;
              # For high-availability.  Allows to bind NGINX without to a
              # specific IP address without this IP address assigned on a
              # host.
              "net.ipv4.ip_nonlocal_bind" = 1;
            };

            systemd = {
              services = {
                containerd = {
                  restartTriggers = [ config.environment.etc."cni/net.d".source ];
                };
                kube-addon-manager.environment.KUBECONFIG = "/etc/kubernetes/cluster-admin.kubeconfig";
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

            environment.systemPackages = with pkgs;
              let
                wrapKubectl = with pkgs;
                  runCommand "wrap-kubectl" { buildInputs = [ makeWrapper ]; } ''
                    mkdir -p $out/bin
                    makeWrapper ${pkgs.kubernetes}/bin/kubectl $out/bin/kubectl --set KUBECONFIG "/etc/kubernetes/cluster-admin.kubeconfig"
                  '';
                wrapEtcd = with pkgs;
                  runCommand "wrap-etcd" { buildInputs = [ makeWrapper ]; } ''
                    mkdir -p $out/bin
                    makeWrapper ${pkgs.etcd}/bin/etcdctl $out/bin/etcdctl --set ETCDCTL_PEERS https://${cfg.hostName}:2379 --set ETCDCTL_CERT_FILE ${cfg.certificates.kubernetes."kubernetes.pem"} --set ETCDCTL_KEY_FILE ${cfg.certificates.kubernetes."kubernetes-key.pem"} --set ETCDCTL_CA_FILE ${cfg.certificates.kubernetes."ca.pem"}
                  '';
              in
              [ wrapKubectl wrapEtcd rakkess ];

            environment.etc = {
              # Required by third-party CNI installed outside of Nix.
              "cni/net.d".enable = false;
              "cni/net.d/05-cilium.conf" = {
                text = builtins.toJSON config.services.kubernetes.cni.cilium.config;
                mode = "0644";
              };

              "kubernetes/cluster-admin.kubeconfig" = {
                source =
                  builtins.toFile "cluster-admin.kubeconfig"
                    (builtins.toJSON {
                      apiVersion = "v1";
                      clusters = [{
                        cluster = {
                          certificate-authority-data = stringOrFile cfg.certificates.kubernetes."ca.pem.base64";
                          server = "https://${cfg.hostName}:6443";
                        };
                        name = "local";
                      }];
                      contexts = [{
                        context = {
                          cluster = "local";
                          user = "admin";
                        };
                        name = "local";
                      }];
                      current-context = "local";
                      kind = "Config";
                      users = [{
                        name = "admin";
                        user = {
                          client-certificate-data = stringOrFile cfg.certificates.kubernetes."admin.pem.base64";
                          client-key-data = stringOrFile cfg.certificates.kubernetes."admin-key.pem.base64";
                        };
                      }];
                    });
                mode = "0600";
                uid = 162;
              };
            };

            programs.bash.interactiveShellInit = ''
              source <(${pkgs.kubernetes}/bin/kubectl completion bash)
            '';

            services = {
              etcd = {
                enable = true;
                keyFile = cfg.certificates.kubernetes."kubernetes-key.pem";
                certFile = cfg.certificates.kubernetes."kubernetes.pem";
                trustedCaFile = cfg.certificates.kubernetes."ca.pem";
                peerClientCertAuth = true;

                # XXX: options.services.etcd.listenClientUrls.default
                listenClientUrls = [ "https://${cfg.bindAddress}:2379" ];

                listenPeerUrls = [ "https://${cfg.bindAddress}:2380" ];
                initialCluster =
                  map
                    (member: "${member.hostName}=https://${member.address}:2380")
                    cfg.cluster;

                initialClusterState = "existing";

                extraConf = {
                  LISTEN_METRICS_URLS = "http://${cfg.bindAddress}:2381";
                };

                name = cfg.hostName;
              };

              flannel = {
                enable = false;
                iface = cfg.bindInterface;
                network = config.services.kubernetes.clusterCidr;
                etcd = {
                  certFile = cfg.certificates.kubernetes."flannel-client.pem";
                  keyFile = cfg.certificates.kubernetes."flannel-client-key.pem";
                  caFile = cfg.certificates.kubernetes."ca.pem";
                  endpoints =
                    map
                      (member: "https://${member.address}:2379")
                      cfg.cluster;
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
                masterAddress = cfg.bindAddress;

                # Will be used in all 'kubeconfig' configurations.
                apiserverAddress = "https://${cfg.hostName}:6443";

                pki = {
                  enable = false;
                };
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
                #
                # Also, it's required for pod/hostpath-provisioner container
                # creation.
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
                  # verbosity.
                  #
                  # --v=2 Useful steady state information about the service
                  # and important log messages that might correlate to
                  # significant changes in the system. This is the commended
                  # default log level.
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
                  extraOpts = "--fail-swap-on=false --v=4 --pod-manifest-path=/etc/kubernetes/manifests";
                  address = cfg.bindAddress;
                  clientCaFile = cfg.certificates.kubernetes."ca.pem";
                  tlsCertFile = cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}.pem";
                  tlsKeyFile = cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}-key.pem";
                  kubeconfig = {
                    certFile = cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}.pem";
                    keyFile = cfg.certificates.kubernetes."kubelet-client-${cfg.hostName}-key.pem";
                  };
                };

                apiserver = {
                  enable = true;
                  clientCaFile = cfg.certificates.kubernetes."ca.pem";
                  kubeletClientCaFile = cfg.certificates.kubernetes."ca.pem";
                  kubeletClientCertFile = cfg.certificates.kubernetes."admin.pem";
                  kubeletClientKeyFile = cfg.certificates.kubernetes."admin-key.pem";
                  proxyClientCertFile = cfg.certificates.kubernetes."kube-proxy.pem";
                  proxyClientKeyFile = cfg.certificates.kubernetes."kube-proxy-key.pem";
                  tlsCertFile = cfg.certificates.kubernetes."kubernetes.pem";
                  tlsKeyFile = cfg.certificates.kubernetes."kubernetes-key.pem";

                  bindAddress = cfg.bindAddress;
                  advertiseAddress = cfg.bindAddress;
                  securePort = 6443;
                  serviceAccountSigningKeyFile = cfg.certificates.kubernetes."service-account-key.pem";
                  serviceAccountKeyFile = cfg.certificates.kubernetes."service-account.pem";
                  etcd = {
                    keyFile = cfg.certificates.kubernetes."kubernetes-key.pem";
                    certFile = cfg.certificates.kubernetes."kubernetes.pem";
                    caFile = cfg.certificates.kubernetes."ca.pem";
                    servers =
                      map
                        (member: "https://${member.address}:2379")
                        cfg.cluster;
                  };

                  allowPrivileged = true; # for kubevirt

                  # Assign a namespace to specific nodes.
                  enableAdmissionPlugins = options.services.kubernetes.apiserver.enableAdmissionPlugins.default ++ [
                    "PodNodeSelector"
                  ];

                  # Required for kubernetes nodes with kubevirt
                  extraOpts = concatStringsSep " " [
                    "--requestheader-client-ca-file=${cfg.certificates.kubernetes."ca.pem"}"
                  ];
                };

                controllerManager = {
                  enable = true;
                  rootCaFile = cfg.certificates.kubernetes."ca.pem";
                  kubeconfig = {
                    certFile = cfg.certificates.kubernetes."kube-controller-manager.pem";
                    keyFile = cfg.certificates.kubernetes."kube-controller-manager-key.pem";
                  };
                  # Override default 127.0.0.1 address for metrics scraping
                  # with Prometheus Kubernetes Operator.
                  address = cfg.bindAddress;
                  # HTTP 403 when accessing kube-controller-manager or
                  # kube-scheduler /metrics · Issue #1903 · rancher/rke2
                  # https://github.com/rancher/rke2/issues/1903
                  #
                  # If you're customizing the configuration to that extent you
                  # would probably also want to set
                  # `--authorization-always-allow-paths=/healthz,/readyz,/livez,/metrics`
                  # (add metrics to the default list) or alternately go
                  # through the work of setting up all the RBAC and CLI flags
                  # related to `--authentication-kubeconfig` so that the
                  # controllers are able to validate the tokens you're
                  # passing. We're going to do that for you on 1.22 but those
                  # changes probably will not be backported to older releases.
                  extraOpts = "--authorization-always-allow-paths=/healthz,/readyz,/livez,/metrics";
                };

                proxy = {
                  enable = false;
                  bindAddress = cfg.bindAddress;
                  kubeconfig = {
                    certFile = cfg.certificates.kubernetes."kube-proxy.pem";
                    keyFile = cfg.certificates.kubernetes."kube-proxy-key.pem";
                  };
                };

                scheduler = {
                  enable = true;
                  kubeconfig = {
                    certFile = cfg.certificates.kubernetes."kube-scheduler.pem";
                    keyFile = cfg.certificates.kubernetes."kube-scheduler-key.pem";
                  };
                  # Override default 127.0.0.1 address for metrics scraping
                  # with Prometheus Kubernetes Operator.
                  address = cfg.bindAddress;
                  # HTTP 403 when accessing kube-controller-manager or
                  # kube-scheduler /metrics · Issue #1903 · rancher/rke2
                  # https://github.com/rancher/rke2/issues/1903
                  #
                  # If you're customizing the configuration to that extent you
                  # would probably also want to set
                  # `--authorization-always-allow-paths=/healthz,/readyz,/livez,/metrics`
                  # (add metrics to the default list) or alternately go
                  # through the work of setting up all the RBAC and CLI flags
                  # related to `--authentication-kubeconfig` so that the
                  # controllers are able to validate the tokens you're
                  # passing. We're going to do that for you on 1.22 but those
                  # changes probably will not be backported to older releases.
                  extraOpts = "--authorization-always-allow-paths=/healthz,/readyz,/livez,/metrics";
                };

                addonManager = {
                  enable = false;
                };
              };

              nginx = {
                enable = true;
                httpConfig = " ";
                streamConfig = ''
                  upstream kube-apiserver {
                      server ${cfg.bindAddress}:6443;
                  }
                '';
              };

              keepalived = {
                enable = true;
                vrrpScripts = {
                  # Check if Ingress controller is alive by connecting to a
                  # kube-apiserver instance running on a local networking
                  # interface.
                  httpHealthCheck = {
                    script = concatStringsSep " " [
                      "${pkgs.curl}/bin/curl"
                      "--silent"
                      "--output"
                      "/dev/null"
                      "--fail"
                      "--max-time"
                      "1"
                      "--key"
                      cfg.certificates.kubernetes."kubernetes-key.pem"
                      "--cert"
                      cfg.certificates.kubernetes."kubernetes.pem"
                      "--cacert"
                      "${cfg.certificates.kubernetes."ca.pem"}"
                      "--resolve"
                      "${cfg.kubeApiServerDnsName}:6443:${cfg.bindAddress}"
                      "https://${cfg.kubeApiServerDnsName}:6443/healthz"
                    ];
                    interval = 1;
                    user = "root";
                  };
                };
              };
            };

            security.pki.certificateFiles = [
              # This CA certificate fixes the error of unknown CA for requests
              # from kubeapiserver to apiservices.  An apiservice should use
              # this CA.
              inputs.ssl-certificates.lib.ssl.kubernetes."ca.pem"
            ];
          }
        ]
        [
          (map
            (replica: {
              environment.etc."keepalived/credentials/password-vrrp-keepalived-${builtins.toString replica}" = {
                text = ''
                  # Do not modify this file!  It was generated by NixOS
                  # and may be overwritten by future invocations.
                  authentication {
                    auth_type PASS
                    auth_pass ${builtins.toString replica}.${cfg.kubeApiServerDnsName}
                  }
                '';
                mode = "0400";
              };
              services.keepalived = {
                vrrpInstances = {
                  "vrrpKubernetes-${builtins.toString replica}" = {
                    # Initial state. As soon as the other machine(s) come up, an
                    # election will be held and the machine with the highest
                    # "priority" will become MASTER. So the entry here doesn't
                    # matter a whole lot.
                    state = "BACKUP";
                    virtualRouterId = cfg.virtualRouterId + replica;
                    extraConfig = ''
                      include /etc/keepalived/credentials/password-vrrp-keepalived-${builtins.toString replica}
                    '';
                    trackScripts = [ "httpHealthCheck" ];
                  };
                };
              };
            })
            # VRRP instance per master (controll-plane) Kubernetes node.
            [ 0 ]
          )
        ])
    );
}
