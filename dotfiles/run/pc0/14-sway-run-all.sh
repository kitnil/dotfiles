#!/usr/bin/env bash

# set -e
set -x

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg workspace 2'

/home/oleg/bin/nerdctl-remmina &
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg splitv'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; export WAYLAND_DISPLAY=wayland-1; /home/oleg/.guix-home/profile/bin/alacritty -e sh -c "sleep 20; exit"' &
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg resize set height 387px'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg splith'
sleep 2

/home/oleg/bin/nerdctl-nixos-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; exec firefox --profile /home/oleg/.mozilla/firefox/messaging' &
sleep 5

/home/oleg/bin/nerdctl-nixos-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; export DISPLAY=:0; exec chatterino' &
sleep 2

echo "INFO: Sort windows." 1>&2

sway_focus_left()
{
    /home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg focus left'
}

sway_move_right()
{
    /home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg move right'
}

sway_move_left()
{
    /home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg move sway_move_left'
}

echo "INFO: Select Firefox Twitch window." 1>&2
sway_focus_left
sway_focus_left
sway_focus_left
sway_focus_left
sway_focus_left
sway_focus_left

echo "INFO: Move Firefox Twitch window." 1>&2
sway_move_right
sway_move_right
sway_move_right
sway_move_right
sway_move_right

echo "INFO: Select Firefox NUUM window." 1>&2
sway_focus_left
sway_focus_left

echo "INFO: Move Firefox NUUM window." 1>&2
sway_move_left
sway_move_left
sway_move_left
sway_move_left

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg focus up'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg splith'
sleep 2

/home/oleg/bin/nerdctl-nixos-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; exec firefox --profile /home/oleg/.mozilla/firefox/twitch' &
sleep 5

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg splith'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg layout tabbed'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg focus left'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg splith'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg layout tabbed'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg exec obs'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg exec pavucontrol'
sleep 2

/home/oleg/bin/nerdctl-nixos-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; exec firefox --profile /home/oleg/.mozilla/firefox/video' &
sleep 5

/home/oleg/bin/nerdctl-nixos-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; export DISPLAY=:0; exec google-chrome-stable' &
sleep 5

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; export WAYLAND_DISPLAY=wayland-1; /home/oleg/.guix-home/profile/bin/alacritty -e sh -c "sleep 20; exit"' &
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg splitv'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg layout tabbed'
sleep 2

/home/oleg/bin/nerdctl-nixos-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; exec firefox --profile /home/oleg/.mozilla/firefox/pcaaxem9.default' &
sleep 5

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg focus parent'
sleep 2

/home/oleg/bin/nerdctl-archlinux-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; export DISPLAY=:0; exec socialstream' &
sleep 5

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg splitv'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'export SWAYSOCK="$(echo /run/user/1000/sway-ipc*sock)"; /home/oleg/.guix-home/profile/bin/swaymsg layout tabbed'
sleep 2

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'pactl set-default-sink alsa_output.pci-0000_12_00.6.analog-stereo'
sleep 1

/home/oleg/bin/nerdctl-guix-exec /bin/sh -lc 'pactl set-source-volume alsa_input.usb-FIFINE_Microphones_FIFINE_K670_Microphone_REV1.0-00.analog-stereo 80%'
sleep 1

/home/oleg/bin/nerdctl-nixos-exec /bin/sh -lc 'export XDG_RUNTIME_DIR=/mnt/guix/run/user/1000; exec wayvnc --output=HEADLESS-2' &
sleep 5

exec sleep infinity
