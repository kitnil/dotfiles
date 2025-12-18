{
  description = "";

  inputs = {
    original.url = "git+file:/home/oleg/src/cgit.wugi.info/wigust/dotfiles?dir=nix/container-systemd-taskexecutor";
    nixpkgs-21-11.url = "nixpkgs/nixos-21.11";
    github-com-kitnil-nix-ipmiview.url = "git+ssh://gitlab.corp1.majordomo.ru/utils/ipmiview?ref=flake";
  };

  outputs = { self
            , original
            , nixpkgs-21-11
            , github-com-kitnil-nix-ipmiview
            , ... }:
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
                  extraSpecialArgs = {
                    inherit (nixpkgs-21-11.legacyPackages.${system}) robo3t;
                    inherit (github-com-kitnil-nix-ipmiview.packages.${system})
                      ipmiview-wrapper;
                  };
                };
              }
              ./hosts/nixos-systemd.nix
            ];
          };
        };
   };
}

