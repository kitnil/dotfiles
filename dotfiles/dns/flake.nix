#!/usr/bin/env -S bash -c "nix-shell --run 'deploy . -- -L'"

{
  description = "wugi.info DNS zone file";

  inputs = {
    dns = {
      url = "github:kirelagin/dns.nix";
      inputs.nixpkgs.follows = "nixpkgs"; # (optionally)
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, dns, deploy-rs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      inherit (pkgs) lib;
      inherit (lib) attrNames;
      directoryToNixosConfigurations = directory:
        with nixpkgs.legacyPackages.${system};
        with lib;
        foldr
          (host: hosts:
            let
              hostName = removeSuffix ".nix" (builtins.baseNameOf host);
              # TODO: Switch to modules after migrating all webs.
              hostsNix = {
                vm1 = {
                  inherit pkgs;
                  inherit (nixpkgs) lib;
                };
                vm2 = {
                  inherit pkgs;
                  inherit (nixpkgs) lib;
                };
              };
              pkgs = hostsNix."${hostName}".pkgs;
              lib = hostsNix."${hostName}".lib;
            in
            hosts //
            {
              ${(removeSuffix ".nix" (builtins.baseNameOf host))} =
                lib.nixosSystem {
                  inherit pkgs system;
                  modules = [
                    host
                  ];
                  specialArgs = {
                    inherit system inputs;
                    inherit (self) packages;
                  };
                };
            })
          { }
          (lib.filter
            (host: hasSuffix ".nix" host)
            (lib.filesystem.listFilesRecursive directory));
    in
    rec {
      lib = {
        zone = import ./wugi.info.nix { inherit dns; };
      };
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          pkgs.nixFlakes
          deploy-rs.outputs.packages.${system}.deploy-rs
        ];
        shellHook = ''
           # Fix ssh completion
           # bash: warning: setlocale: LC_CTYPE: cannot change locale (en_US.UTF-8)
           export LANG=C

          . ${pkgs.nixUnstable}/share/bash-completion/completions/nix
        '';
      };
      packages.${system} =
        let
          inherit (nixpkgs.legacyPackages.x86_64-linux) pkgs;
          inherit (pkgs) bind writeTextFile;
        in
        rec {
          zone = writeTextFile {
            name = "wugi.info.zone";
            text = dns.lib.toString "wugi.info" lib.zone;
            checkPhase = "${bind}/bin/named-checkzone wugi.info $out";
          };
          default = zone;
        };
      nixosConfigurations = directoryToNixosConfigurations ./hosts;
      deploy.nodes =
        with pkgs;
        with lib;
        let
          dryActivateScript = pkgs.writeScript "deploy-rs-dry-activate" ''
            #!${pkgs.runtimeShell}
            echo $PROFILE
          '';
        in
        builtins.listToAttrs (map
          (host: {
            name = host;
            value = {
              hostname = host;
              profiles = {
                knot-zone-wugi-info = {
                  user = "root";
                  sshUser = "oleg";
                  autoRollback = false;
                  magicRollback = false;
                  path =
                    (deploy-rs.lib.${system}.activate.custom // { dryActivate = dryActivateScript; })
                      (pkgs.symlinkJoin {
                        name = "profile";
                        paths = with lib; [ ];
                      })
                      (pkgs.writeScript "activate" ''
                        set -x
                        zone_file=/var/lib/knot/zones/wugi.info.zone
                        if [[ -f $zone_file ]]
                        then
                            rm "$zone_file"
                        fi
                        ln --symbolic --force ${self.packages.${system}.zone} "$zone_file"
                        /run/current-system/profile/bin/herd restart knot
                      '');
                };
              };
            };
          })
          (attrNames nixosConfigurations));

    };
}
