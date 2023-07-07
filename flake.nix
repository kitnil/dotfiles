{
  description = "Vosk transcription using Python";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };
  
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [ vosk-overlay ];
      };

      vosk-overlay = (final: prev:
        {
          vosk-model-small-en-us = prev.stdenv.mkDerivation rec {
            version = "0.15";
            pname = "vosk-model-small-en-us";
            src = prev.fetchzip {
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
          };
          
          vosk = with prev.python3.pkgs;
            buildPythonPackage rec {
              pname = "Vosk";
              version = "0.3.45";
              format = "wheel";
              src = prev.fetchurl {
                url = "https://files.pythonhosted.org/packages/fc/ca/83398cfcd557360a3d7b2d732aee1c5f6999f68618d1645f38d53e14c9ff/vosk-0.3.45-py3-none-manylinux_2_12_x86_64.manylinux2010_x86_64.whl";
                sha256 = "sha256-JeAlCTxDmdcnj1Q1aO2MxUYKw6S/SMI2c6zh4l0mYZ8=";
              };
              propagatedBuildInputs = [ tqdm srt requests websockets final.vosk-model-small-en-us ];
              meta = with prev.lib; {
                homepage = "https://pypi.org/project/vosk/";
                description = "Offline open source speech recognition API based on Kaldi and Vosk";
              };
              # TODO: Use absolute path for vosk-transcriber
              voskScript = prev.writeShellScript "vosk" ''
              vosk-transcriber \
                --model ${final.vosk-model-small-en-us}/share/vosk/models/vosk-model-small-en-us \
                $@
              '';
              postInstall = ''
              mkdir -p $out/bin
              cp ${voskScript} $out/bin/vosk
              '';
            };
        }
      );
    in
      {
        vosk = pkgs.vosk;
        vosk-model-small-en-us = pkgs.vosk-model-small-en-us;
      };
}
