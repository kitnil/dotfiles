#!/bin/sh

WAYLAND_DISPLAY=wayland-1
export WAYLAND_DISPLAY

exec wayvnc
