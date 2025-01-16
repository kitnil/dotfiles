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
              python-with-te =
                let
                  pkgs = import taskexecutor.inputs.nixpkgs-19-09 {
                    inherit system;
                    overlays = [
                      taskexecutor.inputs.majordomo.overlay
                      taskexecutor.overlay
                    ];
                  };
                in
                  with pkgs;
                  with pkgs.python37mj.pkgs;
                  symlinkJoin {
                    name = "pythonWithTaskexecutor";
                    paths = pkgs.python37mj.withPackages (ps:
                      (with ps; [
                        kombu
                        clamd
                        PyMySQL
                        jinja2
                        schedule
                        psutil
                        pyaml
                        docker
                        pg8000
                        requests
                        alerta
                        attrs
                        giturlparse
                        quota
                        btrfs-progs
                        restic

                        pip
                        pytest
                        pyfakefs
                        gitaskpass
                        self.packages.${system}.python-taskexecutor-local
                      ]));
                  };
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
                { };
              python-taskexecutor-local-wrapper = callPackage
                ({ stdenv }:
                  let
                    script = pkgs.writeScript "python" ''
                      #!/bin/sh

                      unset LD_LIBRARY_PATH

                      HOSTNAME=web99
                      export HOSTNAME

                      CONFIG_PROFILE=staging
                      export CONFIG_PROFILE

                      APIGW_HOST="api.intr"
                      export APIGW_HOST

                      TE_AMQP_HOST="rabbit.intr"
                      export TE_AMQP_HOST

                      LOG_LEVEL="debug"
                      export LOG_LEVEL

                      APIGW_PASSWORD="''${APIGW_PASSWORD}"
                      export APIGW_PASSWORD

                      exec /run/wrappers/bin/sudo -E /etc/profiles/per-user/taskexecutor/bin/python3 "$@"
                    '';
                  in
                  stdenv.mkDerivation {
                    name = "python-taskexecutor-local-wrapper";
                    src = null;
                    dontUnpack = true;
                    installPhase = ''
                      install -Dm555 ${script} $out/bin/python
                    '';
                  })
                { };
            };
   };
}

