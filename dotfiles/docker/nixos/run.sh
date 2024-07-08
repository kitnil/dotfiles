#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

# --security-opt seccomp=unconfined required for keyctl(2)
docker_args=(
    -it
    --name nixos-systemd
    --env "container=docker"
    --tmpfs /tmp
    --tmpfs /run
    --stop-signal SIGRTMIN+3
    --device /dev/fuse
    --security-opt apparmor=unconfined
    --security-opt seccomp=unconfined
    --device /dev/net/tun
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
)
for capability in "${capabilities[@]}"
do
    docker_args+=("--cap-add" "$capability")
done

docker run "${docker_args[@]}" nixos-systemd "$@"

cat <<'EOF'
docker exec -it -u oleg: nixos-1 sudo docker run --rm -it busybox
EOF
