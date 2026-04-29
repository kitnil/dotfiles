# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, config, ... }:

{
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = 1;
  };
  programs.niri.enable = true;
  programs.sway.enable = true;
  programs.gamescope = {
    enable = true;
    capSysNice = false;
  };
  services.bird = {
    enable = true;
    config = lib.readFile ./../bird.1.conf;
    checkConfig = false;
  };
  environment.etc = {
    "bird/bird.conf" = {
      mode = "0644";
    };
    "bird/peers/nixos-hev.conf" = {
      text = lib.readFile ./../peers/nixos-hev.conf;
      mode = "0644";
    };
  };
   systemd.services.bird.reloadTriggers = [
    config.environment.etc."bird/bird.conf".source
    config.environment.etc."bird/peers/nixos-hev.conf".source
  ];
  services.prometheus.exporters.bird = {
    enable = true;
  };
  environment.systemPackages = [
    pkgs.guile
    pkgs.mangohud
    pkgs.mtr
    pkgs.wireshark
  ];
  services.webhook-custom = {
    enable = true;
    flake = ".#container-systemd-nixos-workstation-pc0";
  };

  programs.wireshark.enable = true;
  users.users.oleg.extraGroups = [ "wireshark" ];
  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };

  services.mediamtx = {
    enable = true;
    settings = {
      metrics = true;
      paths = {
        all = {
          source = "publisher";
        };
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    179                         # bgp (bird)
    9150                        # tor browser
    9324                        # prometheus bird exporter
    11434                       # ollama
    31247                       # prometheus mtr exporters
  ];
  systemd.services.reload-systemd-vconsole-setup.enable = false;

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
  };
  services.ollama = {
    enable = true;
    package = pkgs.ollama-vulkan;
    host = "0.0.0.0";
  };

  console.enable = true;
  systemd.services."getty@tty1" = {
    enable = false;
  };
  systemd.services."autovt@tty1" = {
    enable = false;
  };
  systemd.services."getty@tty14" = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
  };
  systemd.services.debug-shell = {
    serviceConfig = {
      TTYReset = false;
      TTYVHangup = false;
      TTYVTDisallocate = false;
    };
  };
  systemd.services.console-getty = {
    enable = false;
    wantedBy = [ ];
  };
  services.getty.autologinUser = "oleg";
  services.logind = {
    settings.Login = {
      NAutoVTs = 0;
      ReserveVT = 0;
    };
  };
}
