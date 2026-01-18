# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

let
  # Copied from
  # github.com/nixos/nixpkgs/nixos/modules/services/cluster/kubernetes/default.nix
  defaultContainerdSettings = {
    version = 2;
    root = "/var/lib/containerd";
    state = "/run/containerd";
    oom_score = 0;

    grpc = {
      address = "/run/containerd/containerd.sock";
    };

    plugins."io.containerd.grpc.v1.cri" = {
      sandbox_image = "docker.io/library/pause:latest";

      cni = {
        bin_dir = "/opt/cni/bin";
        max_conf_num = 0;
      };

      containerd.runtimes.runc = {
        runtime_type = "io.containerd.runc.v2";
        options.SystemdCgroup = true;
      };
    };
  };
in
{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  environment.systemPackages = [
    pkgs.lsof
    pkgs.strace
    pkgs.tcpdump
    pkgs.openssl
  ];
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-kube103";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
  systemd.oomd.enable = false;
  virtualisation.containerd = {
    enable = true;
    settings = lib.mapAttrsRecursive (name: lib.mkDefault) defaultContainerdSettings;
  };
  systemd.services.custom-kubelet = {
    wantedBy = [ "multi-user.target" ];
    after = [
      "network.target"
      "containerd.service"
    ];
    path = [
      pkgs.util-linuxMinimal
      "/opt/cni"
    ];
    serviceConfig = {
      ExecStart = "${pkgs.kubernetes}/bin/kubelet --address=192.168.0.104 --node-ip=192.168.0.104 --authentication-token-webhook --authentication-token-webhook-cache-ttl=10s --authorization-mode=Webhook --client-ca-file=/etc/kubernetes/pki/ca.pem --cluster-dns=10.8.255.254 --cluster-domain=cluster.local --hairpin-mode=hairpin-veth --healthz-bind-address=127.0.0.1 --healthz-port=10248 --hostname-override=kube103 --kubeconfig=/etc/kubernetes/kubeconfig --pod-infra-container-image=pause --port=10250 --register-node=true --register-with-taints=unschedulable=true:NoSchedule --root-dir=/var/lib/kubelet --tls-cert-file=/etc/kubernetes/pki/kubelet-client-kube103.pem --tls-private-key-file=/etc/kubernetes/pki/kubelet-client-kube103-key.pem --container-runtime-endpoint=unix:///run/containerd/containerd.sock --fail-swap-on=false --eviction-hard=nodefs.available<5Gi,nodefs.inodesFree<500000,imagefs.available<5Gi,imagefs.inodesFree<500000 --image-gc-high-threshold=95 --image-gc-low-threshold=90 --pod-manifest-path=/etc/kubernetes/manifests --max-pods=200";
    };
  };
}
