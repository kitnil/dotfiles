{ pkgs }:

let
  openvpn-config = pkgs.writeText "openvpn.conf" ''
    client
    proto udp
    dev tapvpn1
    ca ${./openvpn/ca.crt}
    cert ${./openvpn/client.crt}
    key ${./openvpn/client.key}
    comp-lzo
    persist-key
    persist-tun
    verb 3
    nobind
    ping 5
    ping-restart 10
    resolv-retry infinite
    remote guix.wugi.info 1195
    remote-random
  '';
in pkgs.dockerTools.buildLayeredImageWithNixDb rec {
  name = "localhost:5000/openvpn-client";
  tag = "latest";
  contents = with pkgs; [ bash openvpn ];
  config = {
    Entrypoint = [ "${pkgs.openvpn}/bin/openvpn" "--config" openvpn-config ];
    Env = [
      "TZ=Europe/Moscow"
      "LC_ALL=en_US.UTF-8"
    ];
  };
  extraCommands = ''
    set -x -e

    mkdir -p tmp
    chmod 777 tmp
  '';
}

# iptables -t nat -A POSTROUTING -o ens3 -j MASQUERADE
# iptables -A FORWARD -i ens3 -o tapvpn1 -m state --state RELATED,ESTABLISHED -j ACCEPT
# iptables -A FORWARD -i tapvpn1 -o ens3 -j ACCEPT
