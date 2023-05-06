{
  description = "Windows virtual machine configuration with GPU passthough";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      inherit (pkgs) callPackage;
    in
    {
      packages.${system} = rec {
        default = xml-patch;
        xml-patch = callPackage
          ({ stdenv, fetchurl, runtimeShell, openjdk }:
            stdenv.mkDerivation rec {
              name = "xml-patch";
              version = "0.3.1";
              src = fetchurl {
                url = "https://repo1.maven.org/maven2/com/github/dnault/xml-patch/0.3.1/xml-patch-${version}.jar";
                sha256 = "1jpbxm0x5khc7yp026snbncb6hs096fk2jq6j3daxk1ax948cr69";
              };
              dontUnpack = true;
              installPhase = ''
                # Install jar
                mkdir -p $out/lib/xml-patch
                install -m444 $src $out/lib/xml-patch/xml-patch.jar
                # Install wrapper
                mkdir -p $out/bin
                cat > xml-patch <<EOF
                #!${runtimeShell} -e
                exec -a "$0" ${openjdk}/bin/java -jar $out/lib/xml-patch/xml-patch.jar "\$@"
                EOF
                install -m555 xml-patch $out/bin/xml-patch
              '';
            })
          { openjdk = pkgs.openjdk11; };
      };
    };
}
