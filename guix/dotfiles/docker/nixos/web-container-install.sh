#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

PREFIX="${PREFIX:-/mnt}"

if [[ $(find -L "$PREFIX" | wc -l) -eq 1 ]]
then
    mkdir -p "${PREFIX}/etc/nixos"
    cat > "${PREFIX}/etc/nixos/configuration.nix" <<'EOF'
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = [];

  boot.isContainer = true;
  boot.loader.initScript.enable = true;

  time.timeZone = "Europe/Moscow";

  networking.hostName = ""; # empty
  networking.useDHCP = false;
  networking.useNetworkd = false;
  networking.useHostResolvConf = false;
  networking.firewall.enable = false;
  # virtualisation.docker.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  nix.trustedUsers = [ "root" ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = false;
  services.openssh.permitRootLogin = "yes";

  # Define a user account. Don't forget to set a password with ‘passwd’.

  users.users.root.password = ""; # Empty password.

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
EOF
    (
        set -e
        nixos-install --no-root-password --root "$PREFIX"
    )
else
    echo "${PREFIX} is not empty"
    exit 1
fi
