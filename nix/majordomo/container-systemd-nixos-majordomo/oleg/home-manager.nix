{ pkgs, lib, config, ... }:
{
  imports = [
    ../../modules/services/firefox.nix
    ./private.nix
  ];
  home.packages = [
    pkgs.alacritty
    pkgs.fuzzel
    pkgs.ipmitool
    pkgs.ipmiview
    pkgs.jetbrains.pycharm-oss
    pkgs.mariadb.client
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

  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "xterm-256color";
      };
      font = {
        size = 10;
      };
      general = {
        live_config_reload = false;
      };
      colors = {
        primary = {
          foreground = "0xFFFFFF";
          background = "0x000000";
        };
        normal = {
          black = "0x000000";
          red = "0xB21818";
          green = "0x66CD00";
          yellow = "0xB26818";
          blue = "0x4169e1";
          magenta = "0xB218B2";
          cyan = "0x18B2B2";
          white = "0xB2B2B2";
        };
        bright = {
          black = "0x686868";
          red = "0xFF5454";
          green = "0x54FF54";
          yellow = "0xEEC900";
          blue = "0x5454FF";
          magenta = "0xFF54FF";
          cyan = "0x54FFFF";
          white = "0xFFFFFF";
        };
      };
    };
  };

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

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    defaultCacheTtl = 172800;
    defaultCacheTtlSsh = 172800;
    maxCacheTtl = 172800;
    maxCacheTtlSsh = 172800;
    grabKeyboardAndMouse = false;
    pinentry = {
      package = pkgs.pinentry-curses;
    };
    extraConfig = ''
      allow-preset-passphrase
    '';
  };

  home.file = {
    ".my.cnf" = {
      text = ''
        [client]
        skip-ssl = true
      '';
    };
  };

  # The home.stateVersion option no longer has a default value. It used to
  # default to “18.09”, which was the Home Manager version that introduced the
  # option. If your configuration does not explicitly set this option then you
  # need to add
  home.stateVersion = "24.05";
}
