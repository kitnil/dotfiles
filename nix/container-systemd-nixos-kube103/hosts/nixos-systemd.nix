# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-kube103";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
  systemd.oomd.enable = false;
  services.kubernetes = {
    masterAddress = "kubernetes.home";
  };
  services.kubernetes.addons.dns.enable = false;
  services.kubernetes.kubelet = rec {
    enable = true;
    clientCaFile = "/etc/kubernetes/pki/ca.pem";
    tlsCertFile = "/etc/kubernetes/pki/kubelet-client-kube103.pem";
    tlsKeyFile = "/etc/kubernetes/pki/kubelet-client-kube103-key.pem";
    extraConfig = {
      failSwapOn = false;
    };
    kubeconfig = {
      caFile = clientCaFile;
      certFile = tlsCertFile;
      keyFile = tlsKeyFile;
    };
  };
}
