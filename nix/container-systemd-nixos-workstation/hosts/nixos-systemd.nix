# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, lib, ... }:

{
  services.udev = {
    enable = lib.mkForce true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = false;
  services.openssh.settings = {
    PermitRootLogin = "prohibit-password";
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
  systemd.services.reload-systemd-vconsole-setup.enable = false;
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

  local.services.prometheus.exporters.blackbox = {
    enable = true;
  };
}
