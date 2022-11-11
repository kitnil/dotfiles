{
  description = "wugi.info DNS zone file";

  inputs = {
    dns = {
      url = "github:kirelagin/dns.nix";
      inputs.nixpkgs.follows = "nixpkgs"; # (optionally)
    };
  };

  outputs = { self, nixpkgs, dns, ... }: rec {
    lib = {
      zone = import ./wugi.info.nix { inherit dns; };
    };
    packages.x86_64-linux =
      let
        inherit (nixpkgs.legacyPackages.x86_64-linux) pkgs;
        inherit (pkgs) bind writeTextFile;
      in
      {
        default = writeTextFile {
          name = "wugi.info.zone";
          text = dns.lib.toString "wugi.info" lib.zone;
          checkPhase = "${bind}/bin/named-checkzone wugi.info $out";
        };
      };
  };
}
