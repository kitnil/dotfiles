{
  description = "";

  inputs = {
    original.url = "git+file:///home/oleg/.local/share/chezmoi?dir=dotfiles/nix/container-systemd";
    taskexecutor.url = "git+ssh://gitlab.corp1.majordomo.ru/hms/taskexecutor?ref=staging";
    ssl-certificates.url = "git+ssh://git@gitlab.intr/office/ssl-certificates";
  };

  outputs = { self, original, ssl-certificates, taskexecutor, ... }:
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
                    taskexecutor = ./home-manager.nix;
                  };
                  extraSpecialArgs = {
                    python-taskexecutor = self.outputs.packages.${system}.python-with-te;
                  };
                };
              }
              self.nixosModules.taskexecutor-nginx
              ./hosts/nixos-systemd.nix
            ];
            specialArgs = {
              majordomo-tls = {
                certificate = ssl-certificates.outputs.lib.ssl."majordomo.ru.pem";
                key = ssl-certificates.outputs.lib.ssl."majordomo.ru.key";
              };
            };
          };
        };
        nixosModules = {
          taskexecutor-nginx = import ./modules/services/taskexecutor-nginx.nix;
        };
        packages.${system} =
          let
            pkgs = import original.inputs.nixpkgs { inherit system; };
            inherit (pkgs) callPackage;
          in
            rec {
              python-taskexecutor-local = callPackage
                ({ stdenv }: stdenv.mkDerivation {
                  name = "python-taskexecutor-local";
                  src = null;
                  dontUnpack = true;
                  installPhase = ''
                    mkdir -p $out/lib/python3.7/site-packages
                    ln -s /home/oleg/src/gitlab.intr/hms/taskexecutor/src/python/taskexecutor $out/lib/python3.7/site-packages/taskexecutor
                  '';
                })
                {};
              python-with-te = pkgs.symlinkJoin {
                name = "profile";
                paths = [
                  taskexecutor.outputs.packages.${system}.pythonWithTaskexecutor
                  python-taskexecutor-local
                ];
              };
            };
      };
}

