#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

docker run --network=host --dns 8.8.8.8 --detach --cap-add SYS_ADMIN --cap-add SYS_TTY_CONFIG --cap-add SYS_NICE --cap-add NET_BIND_SERVICE --cap-add SYS_TIME --cap-add NET_RAW --cap-add NET_ADMIN --name socialstream --env container=docker --tmpfs /tmp --tmpfs /run --device /dev/dri --stop-signal SIGRTMIN+3 socialstream

sleep 10
docker exec -d -e WAYLAND_DISPLAY=wayland-1 -e XDG_RUNTIME_DIR=/run/user/1000 -e DISPLAY=:0 -uoleg: -it socialstream /home/oleg/squashfs-root/AppRun
