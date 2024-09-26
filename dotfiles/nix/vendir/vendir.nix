{ pkgs }:

pkgs.dockerTools.buildLayeredImage rec {
  name = "harbor.home.wugi.info/library/vendir";
  tag = "latest";
  contents = [ pkgs.vendir ];
  config = {
    Entrypoint = [ "/bin/vendir" ];
  };
}
