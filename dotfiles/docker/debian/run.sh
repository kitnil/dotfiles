#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

shared_directory="${HOME}/.local/docker/debian"
mkdir -p "$shared_directory"

get_pulseaudio_socket_file()
{
    LC_ALL=C pactl info \
        | grep 'Server String' \
        | rev \
        | cut -d' ' -f1 \
        | rev
}
pulseaudio_socket_file="$(get_pulseaudio_socket_file)"
pulseaudio_config_file="${shared_directory}/pulseaudio.client.conf"

cat > "$pulseaudio_config_file" << EOF
default-server = unix:/tmp/pulseaudio.socket
# Prevent a server running in the container

autospawn = no
daemon-binary = /bin/true
# Prevent the use of shared memory

enable-shm = false
EOF

docker_args=(
    --detach
    --dns 8.8.8.8
    --device /dev/dri
    --device /dev/tty6
    --name debian-1
    --env "container=docker"
    --tmpfs /tmp
    --tmpfs /run
    --stop-signal SIGRTMIN+3
    --device /dev/input
    --device /dev/fuse
    --volume "${pulseaudio_socket_file}:/tmp/pulseaudio.socket:ro"
    --volume "${pulseaudio_config_file}:/etc/pulse/client.conf:ro"
    --volume /run/user/1000/wayland-1:/tmp/wayland-1
    --volume /tmp/.X11-unix/X0:/tmp/.X11-unix/X0
)

capabilities=(
    NET_ADMIN
    NET_BIND_SERVICE
    NET_RAW
    SYS_ADMIN
    SYS_NICE
    SYS_TIME
    SYS_TTY_CONFIG
)
for capability in "${capabilities[@]}"
do
    docker_args+=("--cap-add" "$capability")
done

docker run "${docker_args[@]}" debian-systemd

cat <<'EOF'
docker exec -u oleg: debian-1 bash -c 'sudo mkdir -p /run/user/1000/; sudo chown oleg: /run/user/1000/; sudo ln -s /tmp/wayland-1 /run/user/1000/; DISPLAY=:0 WAYLAND_DISPLAY=wayland-1 yandex-browser'
EOF
