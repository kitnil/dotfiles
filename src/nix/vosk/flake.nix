{
  description = "Vosk transcription using Python";

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
          vosk-model-small-en-us = final.callPackage ./vosk-model-small-en-us.nix { };
          vosk-model-small-ru = final.callPackage ./vosk-model-small-ru.nix { };
          vosk = final.python3Packages.callPackage ./vosk.nix { };
        }
      );
    in
    {
      vosk = pkgs.vosk;
      vosk-model-small-en-us = pkgs.vosk-model-small-en-us;
      vosk-model-small-ru = pkgs.vosk-model-small-ru;
    };
}
