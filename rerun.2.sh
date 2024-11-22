#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

ssh 192.168.0.192 <<'EOF'
bar()
{
if sudo /home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg -t get_outputs'
then
    sudo herd restart container-guix-sway-autostart
    exit 0
fi
}
until bar
do
    sleep 2
done
EOF
