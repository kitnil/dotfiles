{
  description = "";

  inputs = {
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = with nixpkgs.legacyPackages."${system}"; mkShell {
        buildInputs = [
          nixFlakes
        ];
        shellHook = ''
          . ${nixFlakes}/share/bash-completion/completions/nix
          export LANG=C
        '';
      };
    })
    // (let
      system = "x86_64-linux";
      inherit (nixpkgs.legacyPackages.${system})
        callPackage;
    in
      {
        packages.${system} = rec {
          tor-bridges = callPackage
            ({ python3, python3Packages }:
              python3.pkgs.buildPythonPackage rec {
                pname = "tor-bridges";
                version = "0.0.1";
                src = ./.;
                doCheck = false;
                propagatedBuildInputs = with python3Packages; [
                  imapclient
                ];
              })
            {};
          container = callPackage
            ({ dockerTools, tor-bridges, cacert }:
              dockerTools.buildLayeredImage {
                name = "harbor.home.wugi.info/library/tor-bridges";
                tag = "latest";
                contents = [
                  cacert
                  cacert.unbundled
                ];
                config = {
                  Entrypoint = [ "${tor-bridges}/bin/tor-bridges" ];
                };
                extraCommands = ''
                  mkdir -p {root,tmp}
                  chmod 777 tmp
                '';
              })
            { inherit tor-bridges; };
        };
        defaultPackage.${system} = self.packages.${system}.container;
      });
}
