#!/bin/sh
mkdir -p nixos
PREFIX=nixos web-container-install.sh
docker build -t nixos-systemd .
