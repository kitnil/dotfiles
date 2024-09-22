#!/bin/sh

docker_args=(
    --device /dev/dri
    --device /dev/fuse
    --device /dev/input
    --device "/dev/${CONTAINER_TTY:-tty2}"
    --device "/dev/${CONTAINER_TTY:-tty2}":/dev/tty0
    --tty
    --volume /etc/nsswitch.conf:/etc/nsswitch.conf:ro
    --volume /etc/services:/etc/services:ro
    --interactive
)

capabilities=(
    SYS_ADMIN
)
for capability in "${capabilities[@]}"
do
    docker_args+=("--cap-add" "$capability")
done

docker run "${docker_args[@]}" "${CONTAINER_IMAGE:-harbor.home.wugi.info/library/guix-image-workstation:latest}" "$@"
