#!/bin/sh
docker run --publish 127.0.0.1:890:1080 --detach --cap-add SYS_ADMIN --cap-add SYS_TTY_CONFIG --cap-add SYS_NICE --cap-add NET_BIND_SERVICE --cap-add SYS_TIME --cap-add NET_RAW --cap-add NET_ADMIN --name byedpi --env container=docker --tmpfs /tmp --tmpfs /run --stop-signal SIGRTMIN+3 byedpi
docker exec archlinux-1 systemctl start byedpi.service
