{
  description = "";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = with nixpkgs.legacyPackages."${system}"; mkShell {
        buildInputs = [
          nixFlakes
          nixos-install-tools
        ];
        shellHook = ''
          . ${nixFlakes}/share/bash-completion/completions/nix
          export LANG=C
        '';
      };
    })
    // (let
      system = "x86_64-linux";
    in
      {
        nixosConfigurations = {
          nixos-systemd = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [ ./hosts/nixos-systemd.nix ];
          };
        };
      });
}
