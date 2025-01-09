{ pkgs }:

pkgs.dockerTools.buildLayeredImage rec {
  name = "harbor.home.wugi.info/library/vendir";
  tag = "latest";
  contents = [
    pkgs.vendir
    pkgs.gitAndTools.git
  ];
  config = {
    Entrypoint = [ "/bin/vendir" ];
  };
}
