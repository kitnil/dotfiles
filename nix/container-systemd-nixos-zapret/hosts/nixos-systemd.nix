# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, ... }:

{
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

  networking.firewall.enable = lib.mkForce true;
  services.zapret = {
    enable = true;
    params = [
      "--debug=1"

      # youtube
      "--dpi-desync=multidisorder"
      "--dpi-desync-split-pos=midsld"
      "--dpi-desync-split-pos=host+1"
    ];
    blacklist = [
      "ttvnw.net"
      "twitch.tv"
    ];
  };

  services.bird = {
    enable = true;
    config = builtins.readFile ./bird.conf;
    checkConfig = false;
  };
  networking.firewall.allowedTCPPorts = [ 179 ];

  # services.openvpn.servers = {
  #   client = {
  #     config = ''
  #       client
  #       proto udp
  #       dev tapvpn1
  #       ca /home/oleg/ssl/openvpn-certs/demoCA/cacert.pem
  #       cert /home/oleg/ssl/openvpn-certs/server.crt
  #       key /home/oleg/ssl/openvpn-certs/server.key
  #       dh /home/oleg/ssl/openvpn-certs/dh2048.pem
  #       comp-lzo
  #       persist-key
  #       persist-tun
  #       verb 3
  #       nobind
  #       ping 5
  #       ping-restart 10
  #       resolv-retry infinite
  #       remote vm2.wugi.info 1195
  #       remote-random
  #     '';
  #   };
  # };
}
