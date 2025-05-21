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
      devShell.x86_64-linux = with pkgs; mkShell {
        buildInputs = [
          nixUnstable
          deploy-rs.outputs.packages.x86_64-linux.deploy-rs
        ];
      };
      packages.x86_64-linux = {
        container-openvpn = pkgs.callPackage ./openvpn.nix {};
      };
    };
}
