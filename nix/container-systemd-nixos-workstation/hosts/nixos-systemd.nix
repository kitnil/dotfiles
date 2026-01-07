# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, ... }:

{
  services.udev = {
    enable = lib.mkForce true;
  };

  programs.firejail = {
    enable = true;
  };

  virtualisation.docker = {
    enable = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = false;
  services.openssh.settings = {
    PermitRootLogin = "prohibit-password";
  };

  services.openvpn.servers = {
    client = {
      config =
        let
          mjuh-utils-openvpn = pkgs.fetchFromGitHub {
            owner = "mjuh";
            repo = "utils-openvpn";
            rev = "901b406f27035c641683f8e869919b9e0eb28153";
            hash = "sha256-6cV7uPJbA5YHGgOu/xuKldUCSaiguzPc2nWQFsws3z8=";
          };
          caFile = "${mjuh-utils-openvpn}/etc/openvpn/majordomo-ca.cert";
        in ''
          client
          proto udp
          dev tapvpn
          verb 3

          remote 78.108.87.250 1194 udp
          cipher AES-256-GCM
          data-ciphers AES-256-GCM

          remote-cert-tls server
          ca "${caFile}"
          auth SHA1
          script-security 3
          auth-nocache
          auth-retry nointeract
          ping 5
          ping-restart 10
          auth-user-pass /etc/openvpn/login.conf
        '';
    };
  };

  systemd = {
    services = {
     openvpn-client = {
        unitConfig = {
          ConditionPathExists = [ "/etc/openvpn/login.conf" ];
        };
      };
    };
  };

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      adwaita-fonts
      dejavu_fonts
      wqy_zenhei
    ];
    fontconfig = {
      enable = true;
    };
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  console.enable = true;
  systemd.services."getty@tty1" = {
    enable = false;
  };
  systemd.services."autovt@tty1" = {
    enable = false;
  };
  systemd.services."getty@tty9" = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
  };

  services.seatd = {
    enable = true;
    user = "oleg";
    group = "users";
  };

  services.sunshine = {
    enable = true;
    autoStart = false;
    capSysAdmin = true; # only needed for Wayland -- omit this when using with Xorg
    # openFirewall = true;
  };
}
