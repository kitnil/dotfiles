# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

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
  systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-antifilter.conf".source
    config.environment.etc."bird/peers/nixos-workstation.conf".source
  ];
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
  networking.firewall.allowedTCPPorts = [
    1080
    9050                        # tor
  ];
  networking.firewall.enable = lib.mkForce true;
  services.prometheus.exporters.bird = {
    enable = true;
  };
  services.dante = {
    enable = true;
    config = lib.readFile ./../dante.conf;
  };
  environment.systemPackages = [
    pkgs.lsof
    pkgs.tcpdump
    pkgs.strace
  ];
  systemd.services.dante-direct = let confFile = pkgs.writeText "dante.conf" ''
logoutput: syslog
debug: 9

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
''; in {
    description = "Dante SOCKS v4 and v5 compatible proxy server";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.dante}/bin/sockd -f ${confFile}";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      # Can crash sometimes; see https://github.com/NixOS/nixpkgs/pull/39005#issuecomment-381828708
      Restart = "on-failure";
    };
  };
}
