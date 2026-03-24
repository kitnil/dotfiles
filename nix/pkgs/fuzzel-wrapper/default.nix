{ stdenv, guile }:

stdenv.mkDerivation {
  name = "fuzzel-wrapper";
  src = ./fuzzel-wrapper.scm;
  dontUnpack = true;
  installPhase = ''
    install -Dm555 $src $out/bin/fuzzel-wrapper
  '';
}
