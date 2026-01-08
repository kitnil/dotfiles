# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, ... }:

{
  services.bird = {
    enable = true;
    config = lib.readFile ./../bird.1.conf;
    checkConfig = false;
  };
  environment.etc = {
    "bird/bird.conf" = {
      mode = "0644";
    };
    "bird/peers/nixos-antifilter.conf" = {
      text = lib.readFile ./../peers/nixos-antifilter.conf;
      mode = "0644";
    };
    "bird/peers/nixos-workstation.conf" = {
      text = lib.readFile ./../peers/nixos-workstation.conf;
      mode = "0644";
    };
  };
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  services.tor = {
    enable = true;
    openFirewall = true;
    client = {
      enable = true;
      socksListenAddress = {
        addr = "0.0.0.0";
        port = 9050;
        IsolateDestAddr = true;
      };
    };
    settings.ControlPort = 9051;
  };
  services._3proxy = {
    enable = true;
    confFile =
      let
        remoteSocks5ServerIp = "127.0.0.1";
      in pkgs.writeText "3proxy.conf" ''
        log /tmp/3proxy.log
        logformat "- +_L%t.%.  %N.%p %E %U %C:%c %R:%r %O %I %h %T"
        maxconn 500
        plugin ${pkgs._3proxy}/local/3proxy/libexec/TransparentPlugin.ld.so transparent_plugin
        auth iponly
        allow *
        parent 1000 socks5 ${remoteSocks5ServerIp} 9050 user2 hghjgjhgj
        transparent
        tcppm -i0.0.0.0 8888 127.0.0.1 11111
        maxconn 500
        notransparent
      '';
  };
  networking.firewall.allowedTCPPorts = [
    8888
    9050                        # tor
  ];
  networking.firewall.enable = lib.mkForce true;
  networking.firewall.extraCommands = ''
    iptables -t nat -A PREROUTING -s 192.168.0.0/24 -p tcp --dport 80 -j REDIRECT --to-ports 8888
    iptables -t nat -A PREROUTING -s 192.168.0.0/24 -p tcp --dport 443 -j REDIRECT --to-ports 8888
  '';
}
