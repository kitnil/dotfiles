# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ lib, ... }:

{
  console.enable = true;
  systemd.services."getty@tty1" = {
    enable = false;
  };
  systemd.services."autovt@tty1" = {
    enable = false;
  };
  systemd.services."getty@tty11" = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
  };

  services.seatd = {
    enable = true;
    user = "oleg";
    group = "users";
  };

  virtualisation.docker.enable = lib.mkForce false;

  services.sunshine = {
    enable = true;
    autoStart = false;
    capSysAdmin = true; # only needed for Wayland -- omit this when using with Xorg
    # openFirewall = true;
  };

  networking.firewall.enable = lib.mkForce true;
  services.zapret = {
    enable = true;
    params = [
      "--dpi-desync=fake,disorder2"
      "--dpi-desync-ttl=1"
      "--dpi-desync-autottl=2"
    ];
  };
}
