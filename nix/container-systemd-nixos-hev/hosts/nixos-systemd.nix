# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  services.bird = {
    enable = true;
    config = lib.readFile ./../bird.1.conf;
    checkConfig = false;
  };
  environment.etc = {
    "bird/bird.conf" = {
      mode = "0644";
    };
    "bird/peers/nixos-workstation.conf" = {
      text = lib.readFile ./../peers/nixos-workstation.conf;
      mode = "0644";
    };
    "bird/peers/nixos-majordomo.conf" = {
      text = lib.readFile ./../peers/nixos-majordomo.conf;
      mode = "0644";
    };
    "bird/peers/nixos-dante.conf" = {
      text = lib.readFile ./../peers/nixos-dante.conf;
      mode = "0644";
    };
    "bird/peers/pc0.conf" = {
      text = lib.readFile ./../peers/pc0.conf;
      mode = "0644";
    };
    "bird/peers/pc0-guix-workstation.conf" = {
      text = lib.readFile ./../peers/pc0-guix-workstation.conf;
      mode = "0644";
    };
    "bird/peers/guixsd-guix-workstation.conf" = {
      text = lib.readFile ./../peers/guixsd-guix-workstation.conf;
      mode = "0644";
    };
    "bird/peers/guixsd.conf" = {
      text = lib.readFile ./../peers/guixsd.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-dante.conf".source
    config.environment.etc."bird/peers/nixos-workstation.conf".source
    config.environment.etc."bird/peers/nixos-majordomo.conf".source
    config.environment.etc."bird/peers/pc0.conf".source
    config.environment.etc."bird/peers/pc0-guix-workstation.conf".source
    config.environment.etc."bird/peers/guixsd.conf".source
    config.environment.etc."bird/peers/guixsd-guix-workstation.conf".source
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  environment.systemPackages = [
    pkgs.ipset
    pkgs.lsof
    pkgs.strace
    pkgs.tcpdump
  ];
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-hev";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
  services.hev-socks5-tproxy = {
    enable = true;
    configFile = builtins.toFile "hev-socks5-tproxy.json" (builtins.toJSON {
      dns = { address = "::"; port = 1053; upstream = "192.168.0.192"; };
      main = { workers = 1; };
      socks5 = { address = "192.168.0.110"; port = 1080; udp = "udp"; };
      tcp = { address = "::"; port = 1088; };
      udp = { address = "::"; port = 1088; };
    });
  };
  networking.firewall = {
    extraCommands = ''
      ipset create byp4 hash:net family inet hashsize 2048 maxelem 65536
      ipset add byp4 0.0.0.0/8
      ipset add byp4 10.0.0.0/8
      ipset add byp4 100.64.0.0/10
      ipset add byp4 127.0.0.0/8
      ipset add byp4 169.254.0.0/16
      ipset add byp4 172.16.0.0/12
      ipset add byp4 192.0.0.0/24
      ipset add byp4 192.0.2.0/24
      ipset add byp4 192.88.99.0/24
      ipset add byp4 192.168.0.0/16
      ipset add byp4 198.18.0.0/15
      ipset add byp4 198.51.100.0/24
      ipset add byp4 203.0.113.0/24
      ipset add byp4 224.0.0.0/4
      ipset add byp4 240.0.0.0/4

      iptables -t mangle -N prerouting-hev
      iptables -t mangle -N output-hev
      iptables -t mangle -A PREROUTING -p tcp -m tcp --dport 80 -j prerouting-hev
      iptables -t mangle -A PREROUTING -p udp -m udp --dport 80 -j prerouting-hev
      iptables -t mangle -A PREROUTING -p tcp -m tcp --dport 443 -j prerouting-hev
      iptables -t mangle -A PREROUTING -p udp -m udp --dport 443 -j prerouting-hev
      iptables -t mangle -A OUTPUT -p tcp -m tcp --dport 80 -j output-hev
      iptables -t mangle -A OUTPUT -p udp -m udp --dport 80 -j output-hev
      iptables -t mangle -A OUTPUT -p tcp -m tcp --dport 443 -j output-hev
      iptables -t mangle -A OUTPUT -p udp -m udp --dport 443 -j output-hev
      iptables -t mangle -A output-hev -m mark --mark 0x438 -j RETURN
      iptables -t mangle -A output-hev -m set --match-set byp4 dst -j RETURN
      iptables -t mangle -A output-hev -p tcp -j MARK --set-xmark 0x440/0xffffffff
      iptables -t mangle -A output-hev -p udp -j MARK --set-xmark 0x440/0xffffffff
      iptables -t mangle -A prerouting-hev -m mark --mark 0x438 -j RETURN
      iptables -t mangle -A prerouting-hev -m set --match-set byp4 dst -j RETURN
      iptables -t mangle -A prerouting-hev -p tcp -j TPROXY --on-port 1088 --on-ip 0.0.0.0 --tproxy-mark 0x440/0xffffffff
      iptables -t mangle -A prerouting-hev -p udp -j TPROXY --on-port 1088 --on-ip 0.0.0.0 --tproxy-mark 0x440/0xffffffff
    '';
    extraStopCommands = ''
      ipset destroy byp4

      iptables -t mangle -D PREROUTING -p tcp -m tcp --dport 80 -j prerouting-hev
      iptables -t mangle -D PREROUTING -p udp -m udp --dport 80 -j prerouting-hev
      iptables -t mangle -D PREROUTING -p tcp -m tcp --dport 443 -j prerouting-hev
      iptables -t mangle -D PREROUTING -p udp -m udp --dport 443 -j prerouting-hev
      iptables -t mangle -D OUTPUT -p tcp -m tcp --dport 80 -j output-hev
      iptables -t mangle -D OUTPUT -p udp -m udp --dport 80 -j output-hev
      iptables -t mangle -D OUTPUT -p tcp -m tcp --dport 443 -j output-hev
      iptables -t mangle -D OUTPUT -p udp -m udp --dport 443 -j output-hev
      iptables -t mangle -D output-hev -m mark --mark 0x438 -j RETURN
      iptables -t mangle -D output-hev -m set --match-set byp4 dst -j RETURN
      iptables -t mangle -D output-hev -p tcp -j MARK --set-xmark 0x440/0xffffffff
      iptables -t mangle -D output-hev -p udp -j MARK --set-xmark 0x440/0xffffffff
      iptables -t mangle -D prerouting-hev -m mark --mark 0x438 -j RETURN
      iptables -t mangle -D prerouting-hev -m set --match-set byp4 dst -j RETURN
      iptables -t mangle -D prerouting-hev -p tcp -j TPROXY --on-port 1088 --on-ip 0.0.0.0 --tproxy-mark 0x440/0xffffffff
      iptables -t mangle -D prerouting-hev -p udp -j TPROXY --on-port 1088 --on-ip 0.0.0.0 --tproxy-mark 0x440/0xffffffff
      iptables -t mangle -X prerouting-hev
      iptables -t mangle -X output-hev
    '';
  };
  systemd.services.firewall.path = [ pkgs.ipset ];
}
