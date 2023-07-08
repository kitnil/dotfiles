{
  lib,
  fetchurl,
  buildPythonApplication,
  makeWrapper,
  tqdm,
  srt,
  requests,
  websockets,
  vosk-model-small-en-us,
}:
buildPythonApplication rec {
  pname = "Vosk";
  version = "0.3.45";
  format = "wheel";
  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/fc/ca/83398cfcd557360a3d7b2d732aee1c5f6999f68618d1645f38d53e14c9ff/vosk-0.3.45-py3-none-manylinux_2_12_x86_64.manylinux2010_x86_64.whl";
    sha256 = "sha256-JeAlCTxDmdcnj1Q1aO2MxUYKw6S/SMI2c6zh4l0mYZ8=";
  };
  propagatedBuildInputs = [ tqdm srt requests websockets vosk-model-small-en-us ];
  meta = with lib; {
    homepage = "https://pypi.org/project/vosk/";
    description = "Offline open source speech recognition API based on Kaldi and Vosk";
  };
  nativeBuildInputs = [ makeWrapper ];
  postInstall = ''
              makeWrapper $out/bin/vosk-transcriber $out/bin/vosk \
                --add-flags "--model ${vosk-model-small-en-us}/share/vosk/models/vosk-model-small-en-us"
              '';
}
