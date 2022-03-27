{ pkgs }:

pkgs.dockerTools.buildLayeredImage rec {
  name = "oracle1.local:5000/yggdrasil";
  tag = "latest";
  contents = with pkgs; [ bashInteractive yggdrasil ];
  config = {
    Entrypoint = [ "${pkgs.yggdrasil}/bin/yggdrasil" "-useconffile" ./yggdrasil.conf ];
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
  '';
}
