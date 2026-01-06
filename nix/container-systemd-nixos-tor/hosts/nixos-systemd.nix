# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:

{
  services.bird = {
    enable = true;
    config = builtins.readFile ./bird.conf;
    checkConfig = false;
  };
  services.tor = {
    enable = true;
    openFirewall = true;
    client = {
      enable = true;
    };
    settings.ControlPort = 9051;
  };
}
