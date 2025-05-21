{
  description = "";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    home-manager.url = "github:nix-community/home-manager?ref=release-23.05";
  };

  outputs = { self, nixpkgs, flake-utils, home-manager, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = with nixpkgs.legacyPackages."${system}"; mkShell {
        buildInputs = [
          nixStable
          nixos-install-tools
        ];
        shellHook = ''
          . ${nixStable}/share/bash-completion/completions/nix
          export LANG=C
        '';
      };
    })
    // (let
      system = "x86_64-linux";
    in
      {
        nixosConfigurations = {
          nixos-systemd-builder = nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = {
              inherit nixpkgs;
            };
            modules = [
              home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.oleg = ./home-manager.nix;
                home-manager.extraSpecialArgs = rec {
                  inherit (self) nixosConfigurations;
                  inherit inputs system;
                  pkgs = import nixpkgs {
                    inherit system;
                  };
                };
              }
              ./hosts/nixos-systemd-builder.nix
            ];
          };
        };
      });
}
