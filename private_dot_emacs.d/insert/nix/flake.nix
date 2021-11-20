{
  description = "";

  nixConfig = {
    substituters = [ "https://cache.nixos.intr/" ];
    trustedPublicKeys = [ "cache.nixos.intr:6VD7bofl5zZFTEwsIDsUypprsgl7r9I+7OGY4WsubFA=" ];
  };

  inputs = {
    deploy-rs.url = "github:serokell/deploy-rs";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-utils.url = "github:numtide/flake-utils";
    majordomo.url = "git+https://gitlab.intr/_ci/nixpkgs";
  };

  outputs = { self, flake-utils, nixpkgs, majordomo, deploy-rs, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = with nixpkgs.legacyPackages."\${system}"; mkShell {
        buildInputs = [
          nixUnstable
          deploy-rs.outputs.packages.\${system}.deploy-rs
        ];
        shellHook = ''
          . \${nixUnstable}/share/bash-completion/completions/nix
          export LANG=C
        '';
      };
    })
    // (let
      system = "x86_64-linux";
    in
      {
        # packages.\${system} =
        # checks.\${system} =
        # defaultPackage.\${system} = self.packages.\${system}.
      });
}
