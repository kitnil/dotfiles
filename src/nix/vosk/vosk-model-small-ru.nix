{ fetchzip
, stdenv
}:
stdenv.mkDerivation rec {
  # Model only, used as input for vosk package
  version = "0.22";
  pname = "vosk-model-small-ru";
  src = fetchzip {
    url = "https://alphacephei.com/vosk/models/${pname}-${version}.zip";
    sha256 = "sha256-l6sw0PdX8h3t2ddTcGEvcmIB/pfFgMxdxp+A6ttDW1c=";
  };
  preInstall = ''
    mkdir -p $out/share/vosk/models/${pname}
  '';
  installPhase = ''
    runHook preInstall
    cp -rp  * $out/share/vosk/models/${pname}/
    runHook postInstall
  '';
}
