{ pkgs }:

pkgs.dockerTools.buildLayeredImage rec {
  name = "oracle1.local:5000/tinyproxy";
  tag = "latest";
  contents = with pkgs; [ bashInteractive tinyproxy ];
  config = {
    Entrypoint = [ "${pkgs.tinyproxy}/bin/tinyproxy" "-d" "-c" ./tinyproxy.conf ];
    Env = [
      "TZ=Europe/Moscow"
      "LC_ALL=en_US.UTF-8"
    ];
  };
  extraCommands = ''
    set -x -e
    mkdir -p tmp
    chmod 777 tmp
    mkdir -p var/run
    chmod 755 var/run

    cat > etc/passwd << 'EOF'
    root:!:0:0:System administrator:/root:/bin/sh
    nobody:!:1000:997::/home/alice:${pkgs.shadow}/bin/nologin
    EOF

    cat > etc/group << 'EOF'
    root:!:0:
    nobody:!:997:
    EOF
  '';
}
