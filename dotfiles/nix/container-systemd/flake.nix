{
  description = "";

  inputs = {
    dotfiles-home-manager.url = "git+file:///home/oleg/.local/share/chezmoi?dir=dotfiles/nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, dotfiles-home-manager, ... } @ inputs:
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
            specialArgs = {
              inherit (inputs.dotfiles-home-manager.inputs) nixpkgs;
            };
            modules = [
              inputs.dotfiles-home-manager.inputs.home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.oleg =  inputs.dotfiles-home-manager.outPath + "../../../dotfiles/nix/pc0/home-manager.nix";
                home-manager.sharedModules = [
                  inputs.dotfiles-home-manager.nixosModules.home-manager-firefox
                  inputs.dotfiles-home-manager.nixosModules.home-manager-foot
                  inputs.dotfiles-home-manager.nixosModules.home-manager-vendir
                  inputs.dotfiles-home-manager.nixosModules.home-manager-wayvnc
                ];
                home-manager.extraSpecialArgs = (rec {
                  inherit (self) nixosConfigurations;
                  inherit inputs system;
                  pkgs = inputs.dotfiles-home-manager.inputs.nixpkgs-home-manager.legacyPackages.${system};
                  packages = with inputs.dotfiles-home-manager.inputs;
                    let
                      inherit (dotfiles-home-manager) overlay;
                    in
                    import inputs.dotfiles-home-manager.inputs.nixpkgs {
                      overlays = [ nur.overlay flake-utils-plus.overlay overlay ];
                      inherit system;
                      config = {
                        allowUnfreePredicate = pkg:
                          builtins.elem (nixpkgs.lib.getName pkg) [ "betterttv" ];
                        permittedInsecurePackages = [ "qtwebkit-5.212.0-alpha4" ];
                      };
                    };
                });
              }
              ./hosts/nixos-systemd.nix
            ];
          };
        };
      });
}
