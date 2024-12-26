{
  description = "";

  inputs = {
    original.url = "git+file:///home/oleg/.local/share/chezmoi?dir=dotfiles/nix/container-systemd";
    taskexecutor.url = "git+ssh://gitlab.corp1.majordomo.ru/hms/taskexecutor";
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
                    taskexecutor-packages = taskexecutor.outputs.devShell.${system}.buildInputs;
                    taskexecutor-pkgs = import taskexecutor.inputs.nixpkgs-19-09 {
                      inherit system;
                      overlays = [
                        taskexecutor.inputs.majordomo.overlay
                        taskexecutor.overlay
                      ];
                    };
                  };
                };
              }
              ./hosts/nixos-systemd.nix
            ];
          };
        };
      };
}

