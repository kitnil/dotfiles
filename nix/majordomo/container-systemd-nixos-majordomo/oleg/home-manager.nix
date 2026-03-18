{ pkgs, lib, config, ... }:
{
  imports = [
    ../../modules/services/firefox.nix
    ./private.nix
  ];
  home.packages = [
    pkgs.fuzzel
    pkgs.ipmitool
    pkgs.ipmiview
    pkgs.pass
    pkgs.skopeo
    pkgs.robo3t
    pkgs.xwayland-satellite
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

  systemd.user.services.xwayland-satellite = {
    Unit = {
      Description = "Xwayland-Satellite terminal";
      StartLimitBurst = 5;
      StartLimitIntervalSec = 10;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
    Service = {
      Environment = [
        "XDG_RUNTIME_DIR=/mnt/guix/run/user/%U"
        "WAYLAND_DISPLAY=wayland-1"
      ];
      ExecStart = "${pkgs.xwayland-satellite}/bin/xwayland-satellite";
      Type = "simple";
      Restart = "always";
      RestartSec = "2s";
    };
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
