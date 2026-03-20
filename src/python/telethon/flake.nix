{
  description = "";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
    in {
      packages.${system} = rec {
        python3-telethon = nixpkgs.legacyPackages.${system}.python3.withPackages (python-packages:
          with python-packages; [
            telethon
            python-lsp-server
            pysocks
          ]);
        default = python3-telethon;
      };
    };
}
