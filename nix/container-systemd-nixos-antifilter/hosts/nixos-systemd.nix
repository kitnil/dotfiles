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
    "bird/peers/antifilter.0.conf" = {
      text = lib.readFile ./../peers/antifilter.0.conf;
      mode = "0644";
    };
    "bird/peers/nixos-bview.conf" = {
      text = lib.readFile ./../peers/nixos-bview.conf;
      mode = "0644";
    };
    "bird/peers/nixos-tor.conf" = {
      text = lib.readFile ./../peers/nixos-tor.conf;
      mode = "0644";
    };
    "bird/peers/nixos-zapret.conf" = {
      text = lib.readFile ./../peers/nixos-zapret.conf;
      mode = "0644";
    };
  };
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/antifilter.0.conf".source
    config.environment.etc."bird/peers/nixos-bview.conf".source
    config.environment.etc."bird/peers/nixos-tor.conf".source
    config.environment.etc."bird/peers/nixos-zapret.conf".source
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-antifilter";
  };
  services.dante = {
    enable = true;
    config = ''
      logoutput: syslog
      debug: 0

      external: eth0
      internal: eth0 port = 1080

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
  networking.firewall.allowedTCPPorts = [
    179                         # bgp (bird)
    9324                        # prometheus bird exporter
    31247                       # prometheus mtr exporters
    53                          # dns
  ];
  services.stubby = {
    enable = true;
    settings = pkgs.stubby.passthru.settingsExample // {
      upstream_recursive_servers =
        let
          # https://wiki.nixos.org/wiki/Encrypted_DNS
          #
          # echo | openssl s_client -connect '1.1.1.1:853' 2>/dev/null | openssl x509 -pubkey -noout | openssl pkey -pubin -outform der | openssl dgst -sha256 -binary | openssl enc -base64
          # or
          # kdig -d @1.1.1.1 +tls-ca +tls-host=one.one.one.one example.com
          digest-sha256 = "GP8Knf7qBae+aIfythytMbYnL+yowaWVeD6MoLHkVRg=";
        in [
          {
            address_data = "1.1.1.1";
            tls_auth_name = "cloudflare-dns.com";
            tls_pubkey_pinset = [{
              digest = "sha256";
              value = digest-sha256;
            }];
          }
          {
            address_data = "1.0.0.1";
            tls_auth_name = "cloudflare-dns.com";
            tls_pubkey_pinset = [{
              digest = "sha256";
              value = digest-sha256;
            }];
          }
        ];
    };
  };
}
