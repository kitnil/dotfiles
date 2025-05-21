#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

shared_directory="${HOME}/.local/docker/ubuntu"
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

# --security-opt seccomp=unconfined required for keyctl(2)
docker_args=(
    --detach
    --device /dev/dri
    --name ubuntu-1
    --env "container=docker"
    --tmpfs /tmp
    --tmpfs /run
    --stop-signal SIGRTMIN+3
    --device /dev/input
    --device /dev/fuse
    # --device /dev/tty8
    --volume "${pulseaudio_socket_file}:/tmp/pulseaudio.socket:ro"
    --volume "${pulseaudio_config_file}:/etc/pulse/client.conf:ro"
    --volume /run/user/1000/wayland-1:/tmp/wayland-1
    --volume /tmp/.X11-unix/X0:/tmp/.X11-unix/X0
    --security-opt apparmor=unconfined
    --security-opt seccomp=unconfined
    --device /dev/net/tun
    --volume "${HOME}/src/gitlab.intr/hms/rc-user:${HOME}/src/gitlab.intr/hms/rc-user"
)

capabilities=(
    SETUID
    BLOCK_SUSPEND
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

docker run "${docker_args[@]}" ubuntu-systemd

cat <<'EOF'
docker exec -u ubuntu: ubuntu-1 bash -c 'sudo mkdir -p /run/user/1000/; sudo chown ubuntu: /run/user/1000/; sudo ln -s /tmp/wayland-1 /run/user/1000/; DISPLAY=:0 WAYLAND_DISPLAY=wayland-1 XDG_RUNTIME_DIR=/run/user/1000 alacritty'
docker exec -u ubuntu: ubuntu-1 bash -c 'sudo mkdir -p /run/user/1000/; sudo chown ubuntu: /run/user/1000/; sudo ln -s /tmp/wayland-1 /run/user/1000/; DISPLAY=:0 WAYLAND_DISPLAY=wayland-1 XDG_RUNTIME_DIR=/run/user/1000 MUTTER_DEBUG_DUMMY_MODE_SPECS=1024x768 dbus-run-session -- gnome-shell --nested --wayland'
EOF
