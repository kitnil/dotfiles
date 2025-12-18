{
  description = "";

  inputs = {
    nixpkgs = {
      url = "nixpkgs/nixpkgs-unstable";
    };
    home-manager = {
      url = "git+https://github.com/nix-community/home-manager?ref=release-25.11";
    };
    original = {
      url = "git+file:/home/oleg/src/cgit.wugi.info/wigust/dotfiles?dir=nix/container-systemd";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.dotfiles-home-manager.inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = { self, original, ... }:
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
                };
              }
              ./hosts/nixos-systemd.nix
            ];
          };
        };
   };
}

