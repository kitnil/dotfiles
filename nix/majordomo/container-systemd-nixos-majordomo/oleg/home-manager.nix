{ pkgs, lib, config, ... }:
{
  imports = [
    ../../modules/services/firefox.nix
    ./private.nix
  ];
  home.packages = [
    pkgs.ipmitool
    pkgs.ipmiview
    pkgs.skopeo
    pkgs.robo3t
  ]
  ++ (map (file: pkgs.writeScriptBin (builtins.baseNameOf file) (builtins.readFile file)) [
    ./bash/Majordomo_LLC_Root_CA.crt.sh
    ./bash/mj-hosts.sh
    ./bash/mjru-alerta
    ./bash/mjru-auth
    ./bash/mjru-dns
    ./bash/mjru-docker
    ./bash/mjru-fetch-history
    ./bash/mjru-flake
    ./bash/mjru-git-clone.sh
    ./bash/mjru-grafana
    ./bash/mjru-hms-migrate-web-account
    ./bash/mjru-infa
    ./bash/mjru-office
    ./bash/mjru-vpn.sh
  ]);

  programs.ssh = {
    enable = true;
  };

  programs.bash = {
    bashrcExtra = ''
      . ${./mjru.bash}
    '';
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "24.05";
}
