{
  description = "Control machine via voice";

  inputs = {
    vosk.url = "git+file:///home/oleg/src/github.com/karthink/vosknix";
  };

  outputs = { self, nixpkgs, vosk }:
    let
      system = "x86_64-linux";
    in {
      packages.${system} = rec {
        python3-vosk = nixpkgs.legacyPackages.${system}.python3.withPackages (python-packages:
          with python-packages; [
            aiohttp
            numpy
            pyaudio
            vosk.vosk
            websocket-client
          ]);
        default = python3-vosk;
      };
    };
}
