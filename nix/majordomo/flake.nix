{
  description = "";

  inputs = {
    home-manager = {
      url = "git+https://github.com/nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-21-11.url = "nixpkgs/nixos-21.11";
    github-com-kitnil-nix-ipmiview.url = "git+ssh://gitlab.corp1.majordomo.ru/utils/ipmiview?ref=flake";

    taskexecutor = {
      url = "git+ssh://gitlab.corp1.majordomo.ru/hms/taskexecutor";
      inputs.majordomo = {
        url = "git+https://gitlab.corp1.majordomo.ru/_ci/nixpkgs";
        inputs.shared-http-errors.url = "git+https://gitlab.corp1.majordomo.ru/shared/http_errors";
      };
    };
    ssl-certificates.url = "git+ssh://git@gitlab.corp1.majordomo.ru/office/ssl-certificates";
  };

  outputs = { self
            , nixpkgs
            , home-manager
            , nixpkgs-21-11
            , github-com-kitnil-nix-ipmiview
            , ssl-certificates
            , taskexecutor
            , ... }:
    let
      system = "x86_64-linux";
    in
      {
        packages.${system} =
          let
            pkgs = import nixpkgs {
              inherit system;
            };
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
              python-taskexecutor-wrapper = callPackage
                ({ stdenv, python-with-te }:
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

                      . /home/oleg/secrets

                      HOME_FILESYSTEM_TYPE=dummy
                      export HOME_FILESYSTEM_TYPE

                      TE_CLAMD_HOST="172.16.115.17"
                      export TE_CLAMD_HOST

                      TE_CLAMD_PORT="3310"
                      export TE_CLAMD_PORT

                      # Add to Idea
                      # TE_SCHEDULE_QUOTA-REPORT_UNIX-ACCOUNT_INTERVAL=50
                      # TE_SCHEDULE_QUOTA-REPORT_UNIX-ACCOUNT_EXEC-TYPE=parallel
                      # TE_SCHEDULE_MALWARE-REPORT_UNIX-ACCOUNT_INTERVAL=5
                      # TE_SCHEDULE_BACKUP_UNIX-ACCOUNT_AT=12:00
                      # TE_SCHEDULE_BACKUP_UNIX-ACCOUNT_EXEC-TYPE=parallel
                      # TE_SCHEDULE_BACKUP_UNIX-ACCOUNT_DAILY="True"

                      # Run manually
                      # mount -o bind,rw /nix/store/...-python3-3.7.5-env/lib/python3.7/site-packages /nix/store/...-python3-3.7.5-env/lib/python3.7/site-packages
                      # ln -s /home/oleg/src/gitlab.intr/hms/taskexecutor/src/python/taskexecutor /nix/store/mxhbaafgn691nklljg7752wsnj2q2lzz-python3-3.7.5-env/lib/python3.7/site-packages/

                      exec /run/wrappers/bin/sudo -E ${python-with-te}/bin/python3 "$@"
                    '';
                  in
                  stdenv.mkDerivation {
                    name = "python-taskexecutor-wrapper";
                    src = null;
                    dontUnpack = true;
                    installPhase = ''
                      install -Dm555 ${script} $out/bin/python-taskexecutor-wrapper
                    '';
                  })
                { inherit python-with-te; };
            };
        nixosConfigurations = {
          container-systemd-nixos-majordomo = nixpkgs.lib.nixosSystem {
            specialArgs = {
              majordomo-tls = {
                certificate = ssl-certificates.outputs.lib.ssl."majordomo.ru.pem";
                key = ssl-certificates.outputs.lib.ssl."majordomo.ru.key";
              };
            };
            modules = [
              ({ pkgs, ... }:
                {
                  nixpkgs.config.allowUnfree = true;
                  nixpkgs.system = system;
                  environment.systemPackages = [
                    pkgs.binutils
                    pkgs.ethtool
                    pkgs.iftop
                    pkgs.lsof
                    pkgs.mtr
                    pkgs.strace
                    pkgs.tcpdump
                    pkgs.tmux
                    pkgs.tshark
                  ];
                })
              home-manager.nixosModules.home-manager
              ./container-systemd-taskexecutor/hosts/nixos-systemd.nix
              ./container-systemd-taskexecutor/modules/services/taskexecutor-nginx.nix
              {
                home-manager = {
                  users = {
                    taskexecutor = ./container-systemd-taskexecutor/oleg/home-manager.nix;
                  };
                  extraSpecialArgs = {
                    python-taskexecutor = self.outputs.packages.${system}.python-with-te;
                    python-taskexecutor-wrapper = self.outputs.packages.${system}.python-taskexecutor-wrapper;
                  };
                };
              }
              {
                home-manager = {
                  useGlobalPkgs = true;
                  users = {
                    oleg = ./container-systemd-nixos-majordomo/oleg/home-manager.nix;
                  };
                  extraSpecialArgs = {
                    inherit (nixpkgs-21-11.legacyPackages.${system}) robo3t;
                    inherit (github-com-kitnil-nix-ipmiview.packages.${system})
                      ipmiview-wrapper;
                  };
                };
              }
              ./container-systemd-nixos-majordomo/hosts/nixos-systemd.nix
            ];
          };
        };
   };
}

