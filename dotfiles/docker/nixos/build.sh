#!/bin/sh
set -o nounset -o errexit -o pipefail -o xtrace

# nix develop --command nix build .#nixosConfigurations.nixos-systemd.config.system.build.toplevel

build_directory="$(mktemp -d -t "nixos-systemd.XXXXXXXXXX")"
rsync -a "${PWD}/" "${build_directory}"
cd "${build_directory}" || exit 1
mkdir nixos
mkdir -p nixos/etc/nixos
cp hosts/nixos-systemd.nix nixos/etc/nixos/configuration.nix
nix develop --command nixos-install --no-root-password --root "${PWD}/nixos"
docker build -t nixos-systemd .
