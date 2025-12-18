{ pkgs }:

pkgs.dockerTools.buildLayeredImage rec {
  name = "oracle1.local:5000/bird";
  tag = "latest";
  contents = with pkgs; [ bashInteractive bird2 ];
  config = {
    Entrypoint = [ "${pkgs.bird2}/sbin/bird" "-c" ./bird.conf "-d" ];
    Env = [
      "TZ=Europe/Moscow"
      "LC_ALL=en_US.UTF-8"
    ];
  };
  extraCommands = ''
    set -x -e
    mkdir -p tmp
    chmod 777 tmp
    mkdir -p var/{run,log}
    chmod 755 var/run
  '';
}
