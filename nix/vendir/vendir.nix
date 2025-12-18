{ pkgs }:

pkgs.dockerTools.buildLayeredImage rec {
  name = "harbor.home.wugi.info/library/vendir";
  tag = "latest";
  contents = [
    pkgs.dockerTools.caCertificates
    pkgs.vendir
    pkgs.git
  ];
  config = {
    Entrypoint = [ "/bin/vendir" ];
  };
}
