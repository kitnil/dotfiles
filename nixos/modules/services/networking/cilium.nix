{ config, lib, ... }:

with lib;

let
  cfg = config.services.kubernetes.cni.cilium;
in
{
  options.services.kubernetes.cni.cilium = {
    config = mkOption {
      type = types.attrs;
      default = {
        cniVersion = "0.3.1";
        enable-debug = true;
        log-file = "/var/run/cilium/cilium-cni.log";
        name = "cilium";
        type = "cilium-cni";
      };
    };
    iptables = mkOption {
      default = true;
      type = types.bool;
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.iptables {
      boot.kernelModules = [
        "xt_socket"
        "iptable_nat"
        "iptable_mangle"
        "iptable_raw"
        "iptable_filter"
      ];
      boot.kernelPatches = [
        {
          name = "cilium";
          patch = null;
          extraConfig = ''
            BPF y
            BPF_SYSCALL y
            NET_CLS_BPF y
            BPF_JIT y
            NET_CLS_ACT y
            NET_SCH_INGRESS y
            CRYPTO_SHA1 y
            CRYPTO_USER_API_HASH y
            CGROUPS y
            CGROUP_BPF y
            PERF_EVENTS y
            SCHEDSTATS y
          '';
        }
        {
          name = "cilium-iptables-based-masquerading";
          patch = null;
          extraConfig = ''
            NETFILTER_XT_SET m
            IP_SET m
            IP_SET_HASH_IP m
          '';
        }
        {
          name = "cilium-l7-and-fqdn-policies";
          patch = null;
          extraConfig = ''
            NETFILTER_XT_TARGET_TPROXY m
            NETFILTER_XT_TARGET_CT m
            NETFILTER_XT_MATCH_MARK m
            NETFILTER_XT_MATCH_SOCKET m
          '';
        }
        {
          name = "cilium-bandwidth-manager";
          patch = null;
          extraConfig = ''
            NET_SCH_FQ m
          '';
        }
        {
          name = "cilium-ipsec";
          patch = null;
          extraConfig = ''
            XFRM y
            XFRM_OFFLOAD y
            XFRM_STATISTICS y
            XFRM_ALGO m
            XFRM_USER m
            INET_ESP m
            INET_IPCOMP m
            INET_XFRM_TUNNEL m
            INET_TUNNEL m
            INET6_ESP m
            INET6_IPCOMP m
            INET6_XFRM_TUNNEL m
            INET6_TUNNEL m
            CRYPTO_AEAD m
            CRYPTO_AEAD2 y
            CRYPTO_GCM m
            CRYPTO_SEQIV m
            CRYPTO_CBC m
            CRYPTO_HMAC y
            CRYPTO_SHA256 y
            CRYPTO_AES m
          '';
        }
      ];
    })
  ];
}
