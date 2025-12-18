{
  description = "";

  inputs = {
    original.url = "git+file:/home/oleg/src/cgit.wugi.info/wigust/dotfiles?dir=nix/container-systemd";
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

