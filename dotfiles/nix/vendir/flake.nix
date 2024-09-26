#!/usr/bin/env -S bash -c "nix-shell --run 'deploy . -- -L'"

{
  description = "Nix package manifest";
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs}:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShell.${system} = with pkgs; mkShell {
        buildInputs = [ nixFlakes ];
      };
      packages.${system} = {
        container-vendir = pkgs.callPackage ./vendir.nix {};
      };
      defaultPackage.${system} = self.packages.${system}.container-vendir;
    };
}
