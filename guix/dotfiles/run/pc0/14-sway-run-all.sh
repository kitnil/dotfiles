#!/usr/bin/env bash

# set -e
set -x

container_id_guix="$(nerdctl -n k8s.io ps --format=json | /home/oleg/.guix-profile/bin/jq --exit-status --raw-output '. | select (.Names | startswith("k8s://workstation/workstation-kube3/guix")) | .ID')"
container_id_archlinux="$(nerdctl -n k8s.io ps --format=json | /home/oleg/.guix-profile/bin/jq --exit-status --raw-output '. | select (.Image | startswith("harbor.home.wugi.info/library/archlinux-systemd")) | .ID')"
container_id_nixos="$(nerdctl -n k8s.io ps --format=json | /home/oleg/.guix-profile/bin/jq --exit-status --raw-output '. | select (.Names | startswith("k8s://workstation/workstation-kube3/nixos")) | .ID')"

nerdctl_guix_exec()
{
    nerdctl -n k8s.io exec -u 1000:998 --interactive "$container_id_guix" "$@"
}

nerdctl_archlinux_exec()
{
    nerdctl -n k8s.io exec -u 1000:998 --interactive "$container_id_archlinux" "$@"
}

nerdctl_nixos_exec()
{
    nerdctl -n k8s.io exec "$container_id_nixos" /run/current-system/sw/bin/machinectl shell --setenv=WAYLAND_DISPLAY=wayland-1 --setenv=XDG_RUNTIME_DIR=/mnt/guix/run/user/1000 oleg@ "$@"
}

nerdctl_guix_exec /bin/sh -l <<'EOF'
sway_pid="$(/run/current-system/profile/bin/pidof sway)"

SWAYSOCK="$(echo /run/user/1000/sway-ipc*${sway_pid}*sock)"
export SWAYSOCK

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

PATH=/home/oleg/.guix-home/profile/bin:/run/current-system/profile/bin
export PATH

alacritty -e sh -c "sleep 20; exit" &
sleep 0.4

swaymsg splitv
sleep 0.4

alacritty -e sh -c "sleep 10; exit" &
sleep 0.4

swaymsg resize set height 387px
sleep 0.4

swaymsg splith
sleep 0.4
EOF

# login
nerdctl -n k8s.io exec "$container_id_archlinux" /bin/machinectl shell --setenv=WAYLAND_DISPLAY=wayland-1 --setenv=XDG_RUNTIME_DIR=/mnt/guix/run/user/1000 oleg@ /bin/sleep infinity &

sleep 0.4

# login
nerdctl -n k8s.io exec "$container_id_nixos" /run/current-system/sw/bin/machinectl shell oleg@ /run/current-system/sw/bin/sleep infinity &

sleep 0.4

nerdctl_nixos_exec /run/current-system/sw/bin/systemctl --user start firefox@messaging
sleep 3

nerdctl_nixos_exec /run/current-system/sw/bin/systemctl --user start chatterino
sleep 0.4

nerdctl_guix_exec /bin/sh -l <<'EOF'
sway_pid="$(/run/current-system/profile/bin/pidof sway)"

SWAYSOCK="$(echo /run/user/1000/sway-ipc*${sway_pid}*sock)"
export SWAYSOCK

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

PATH=/home/oleg/.guix-home/profile/bin:/run/current-system/profile/bin
export PATH

swaymsg focus up
sleep 0.4

swaymsg splith
sleep 0.4
EOF

nerdctl_nixos_exec /run/current-system/sw/bin/systemctl --user start firefox@twitch
sleep 1

nerdctl_guix_exec /bin/sh -l <<'EOF'
sway_pid="$(/run/current-system/profile/bin/pidof sway)"

SWAYSOCK="$(echo /run/user/1000/sway-ipc*${sway_pid}*sock)"
export SWAYSOCK

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

PATH=/home/oleg/.guix-home/profile/bin:/run/current-system/profile/bin
export PATH

swaymsg splith
sleep 0.4

swaymsg layout tabbed
sleep 0.4
EOF

nerdctl_nixos_exec /run/current-system/sw/bin/systemctl --user start google-chrome
sleep 1

nerdctl_guix_exec /bin/sh -l <<'EOF'
sway_pid="$(/run/current-system/profile/bin/pidof sway)"

SWAYSOCK="$(echo /run/user/1000/sway-ipc*${sway_pid}*sock)"
export SWAYSOCK

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

PATH=/home/oleg/.guix-home/profile/bin:/run/current-system/profile/bin
export PATH

swaymsg focus parent
sleep 0.4

swaymsg focus left
sleep 0.4

swaymsg splith
sleep 0.4

swaymsg layout tabbed
sleep 0.4
EOF

nerdctl_nixos_exec /run/current-system/sw/bin/systemctl --user start firefox@pcaaxem9.default
sleep 0.4

nerdctl_archlinux_exec /usr/bin/systemctl --user start socialstream
sleep 1

nerdctl_guix_exec /bin/sh -l <<'EOF'
sway_pid="$(/run/current-system/profile/bin/pidof sway)"

SWAYSOCK="$(echo /run/user/1000/sway-ipc*{sway_pid}*sock)"
export SWAYSOCK

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

PATH=/home/oleg/.guix-home/profile/bin:/run/current-system/profile/bin
export PATH

swaymsg splitv
sleep 0.4
swaymsg layout tabbed
sleep 0.4
EOF

nerdctl_guix_exec /bin/sh -l <<'EOF'
sway_pid="$(/run/current-system/profile/bin/pidof sway)"

SWAYSOCK="$(echo /run/user/1000/sway-ipc*${sway_pid}*sock)"
export SWAYSOCK

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

PATH=/home/oleg/.guix-home/profile/bin:/run/current-system/profile/bin
export PATH

swaymsg splitv
sleep 0.4

swaymsg layout tabbed

pactl set-default-sink alsa_output.pci-0000_12_00.6.analog-stereo

pactl set-source-volume alsa_input.usb-FIFINE_Microphones_FIFINE_K670_Microphone_REV1.0-00.analog-stereo 80%
EOF

exec sleep infinity
