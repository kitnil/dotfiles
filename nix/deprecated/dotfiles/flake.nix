{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, ... }: {
    packages.x86_64-linux = rec {
      dotfiles = nixpkgs.legacyPackages.x86_64-linux.callPackage (
        { stdenv
        , autoconf
        , automake
        , git
        , nixVersions
        , guile
        , pkg-config
        , skopeo
        , yamlfmt
        , yq
        }:
        stdenv.mkDerivation {
          pname = "dotfiles";
          version = "0.0.1";
          src = false;
          dontUnpack = true;
          buildInputs = [
            autoconf
            automake
            git
            guile
            guile.dev
            nixVersions.git
            pkg-config
            skopeo
            yamlfmt
            yq
          ];
        }) { };
      default = dotfiles;
    };
  };
}
