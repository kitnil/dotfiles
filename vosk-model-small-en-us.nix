{
  fetchzip,
  stdenv
}:
stdenv.mkDerivation rec {
  # Model only, used as input for vosk package
  version = "0.15";
  pname = "vosk-model-small-en-us";
  src = fetchzip {
    url = "https://alphacephei.com/vosk/models/vosk-model-small-en-us-0.15.zip";
    sha256 = "sha256-CIoPZ/krX+UW2w7c84W3oc1n4zc9BBS/fc8rVYUthuY=";
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
