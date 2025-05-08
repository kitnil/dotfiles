#!/usr/bin/env bash
set -ex
hash="$(docker container create --entrypoint /bin/sh harbor.home.wugi.info/library/nixos-systemd-builder:c835c4fd)"
mkdir rootfs
docker export "$hash" | tar -C rootfs -x
