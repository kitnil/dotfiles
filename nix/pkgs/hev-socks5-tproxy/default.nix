{ fetchFromGitHub, stdenv }:

stdenv.mkDerivation rec {
  pname = "hev-socks5-tproxy";
  version = "2.10.0";
  src = fetchFromGitHub ({
    owner = "heiher";
    repo = "hev-socks5-tproxy";
    rev = "2.10.0";
    sha256 = "sha256-qPWfWdNCx2F8LFUlOxzM1o4JMFXlLd9s1TCdzjDKQ8c=";
  });
  makeFlags = [
    "INSTDIR=${placeholder "out"}"
  ];
}
