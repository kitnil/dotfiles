{
  description = "";

  inputs = {
    original.url = "git+file:///home/oleg/.local/share/chezmoi?dir=dotfiles/nix/container-systemd";
    taskexecutor.url = "git+ssh://gitlab.corp1.majordomo.ru/hms/taskexecutor?ref=staging";
  };

  outputs = { self, original, taskexecutor, ... }:
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
                    python-taskexecutor = taskexecutor.outputs.packages.${system}.pythonWithTaskexecutor;
                  };
                };
              }
              ./hosts/nixos-systemd.nix
            ];
          };
        };
        packages.${system} =
          let
            pkgs = import original.inputs.nixpkgs { inherit system; };
            inherit (pkgs) callPackage;
          in
            {
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
            };
      };
}

