{
  description = "";

  inputs = {
    original = {
      url = "git+file:/home/oleg/src/cgit.wugi.info/wigust/dotfiles?dir=nix/container-systemd";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, original, nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in
      original.outputs // {
        nixosConfigurations = {
          nixos-systemd = original.nixosConfigurations.nixos-systemd.extendModules {
            modules = [
              {
                home-manager = {
                  users = {
                    oleg = ./oleg/home-manager.nix;
                  };
                  sharedModules = [
                    original.inputs.dotfiles-home-manager.nixosModules.home-manager-chatterino
                  ];
                };
              }
              ./hosts/nixos-systemd.nix
            ];
          };
        };
   };
}

