{ pkgs }:

pkgs.dockerTools.buildLayeredImage rec {
  name = "docker-registry.wugi.info/networking/3proxy";
  tag = "latest";
  contents = [ pkgs._3proxy ];
  config = {
    Entrypoint = [ "/bin/3proxy" "/etc/3proxy.cfg" ];
  };
}
