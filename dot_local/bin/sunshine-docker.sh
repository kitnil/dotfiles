#!/usr/bin/env bash

set -o errexit -o pipefail

sunshine_directory="${SUNSHINE_DIRECTORY:-${HOME}/.config/sunshine}"

image="ghcr.io/lizardbyte/sunshine:9a3553d-archlinux"

docker_arguments=(
    --device /dev/dri/
    --device /dev/input/
    --device /dev/uinput
    --device /dev/snd/
    --env TZ=Europe/Moscow
    --env WAYLAND_DISPLAY="$WAYLAND_DISPLAY"
    --env XDG_ACTIVATION_TOKEN="$XDG_ACTIVATION_TOKEN"
    --env XDG_CACHE_HOME="$XDG_CACHE_HOME"
    --env XDG_CONFIG_DIRS="$XDG_CONFIG_DIRS"
    --env XDG_CONFIG_HOME="$XDG_CONFIG_HOME"
    --env XDG_DATA_DIRS="$XDG_DATA_DIRS"
    --env XDG_DATA_HOME="$XDG_DATA_HOME"
    --env XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR"
    --env XDG_SEAT="$XDG_SEAT"
    --env XDG_SESSION_CLASS="$XDG_SESSION_CLASS"
    --env XDG_SESSION_ID="$XDG_SESSION_ID"
    --env XDG_SESSION_TYPE="$XDG_SESSION_TYPE"
    --env XDG_STATE_HOME="$XDG_STATE_HOME"
    --env XDG_VTNR="$XDG_VTNR"
    --name="$(basename "$sunshine_directory")"
    --network=host
    --rm
    --user="${UID}:998"
    --volume "${sunshine_directory}:/config"
    --volume "${HOME}:${HOME}"
    --volume "/run/user/${UID}:/run/user/${UID}"
    --volume /run/udev:/run/udev
    --volume "${HOME}/passwd:/etc/passwd:ro"
    --volume /etc/group:/etc/group:ro
    --volume "${HOME}/shadow:/etc/shadow:ro"
    --volume /etc/sudoers:/etc/sudoers:ro
    --entrypoint /bin/sudo
    --detach
    "$image"
    --user oleg --preserve-env /bin/sh -e -c
    "sudo ln -s /dev/dri/renderD129 /dev/dri/renderD128; exec /usr/bin/sunshine ${sunshine_directory}/sunshine.conf"
)

exec /run/current-system/profile/bin/docker run "${docker_arguments[@]}"
