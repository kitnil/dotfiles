# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  console.enable = true;
  systemd.services."getty@tty1" = {
    enable = false;
  };
  systemd.services."autovt@tty1" = {
    enable = false;
  };
  systemd.services."getty@tty11" = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
  };

  services.seatd = {
    enable = true;
    user = "oleg";
    group = "users";
  };

  virtualisation.docker.enable = lib.mkForce false;

  networking.firewall = {
    enable = lib.mkForce true;
    extraCommands = ''
      iptables -t nat -A POSTROUTING -s 192.168.0.0/24 -o eth0 -j MASQUERADE
    '';
    extraStopCommands = ''
      iptables -t nat -D POSTROUTING -s 192.168.0.0/24 -o eth0 -j MASQUERADE
    '';
  };
  services.zapret = {
    enable = true;
    params = [
      "--debug=1"

      # youtube
      "--dpi-desync=multidisorder"
      "--dpi-desync-split-pos=midsld"
      "--dpi-desync-split-pos=host+1"
    ];
    whitelist = [
      "ggpht.com"
      "googlevideo.com"
      "jnn-pa.googleapis.com"
      "stable.dl2.discordapp.net"
      "wide-youtube.l.google.com"
      "youtu.be"
      "youtube.com"
      "youtubeembeddedplayer.googleapis.com"
      "youtubei.googleapis.com"
      "youtubekids.com"
      "youtube-nocookie.com"
      "youtube-ui.l.google.com"
      "yt3.ggpht.com"
      "yt3.googleusercontent.com"
      "yt4.ggpht.com"
      "ytimg.com"
      "ytimg.l.google.com"
      "yt-video-upload.l.google.com"

      "7tv.app"
    ];
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
    "bird/peers/nixos-antifilter.conf" = {
      text = lib.readFile ./../peers/nixos-antifilter.conf;
      mode = "0644";
    };
    "bird/peers/nixos-dante.conf" = {
      text = lib.readFile ./../peers/nixos-dante.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-antifilter.conf".source
    config.environment.etc."bird/peers/nixos-dante.conf".source
  ];
  systemd.tmpfiles.rules = [
    "f /var/log/bird.log 0644 bird bird -"
  ];
  networking.firewall.allowedTCPPorts = [
    179
    1080
    9324                        # prometheus bird exporter
    31247
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };

  services.openvpn.servers = {
    client = {
      config = ''
        client
        proto udp
        dev tapvpn1
        ca /home/oleg/ssl/openvpn-certs/demoCA/cacert.pem
        cert /home/oleg/ssl/openvpn-certs/server.crt
        key /home/oleg/ssl/openvpn-certs/server.key
        dh /home/oleg/ssl/openvpn-certs/dh2048.pem
        comp-lzo
        persist-key
        persist-tun
        verb 3
        nobind
        ping 5
        ping-restart 10
        resolv-retry infinite
        remote vm2.wugi.info 1195
        remote-random
      '';
    };
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-zapret";
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };

  services.dante = {
    enable = true;
    config = ''
      logoutput: syslog
      debug: 0

      external: eth0
      internal: eth0 port = 1081

      timeout.io: 60

      clientmethod: none
      socksmethod: none
      user.unprivileged: nobody

      client pass {
          from: 0.0.0.0/0 port 1-65535 to: 0.0.0.0/0
      }

      socks pass {
          from: 0.0.0.0/0 to: 0.0.0.0/0
          protocol: tcp udp
      }
    '';
  };
}
