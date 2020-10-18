#!/bin/sh -e
xrandr --setprovideroutputsource 1 0
xrandr --output DP1 --off --output DP2 --off --output DP3 --off --output HDMI1 --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI2 --off --output HDMI3 --off --output VGA1 --off --output VIRTUAL1 --off --output DisplayPort-1-3 --off --output DisplayPort-1-4 --off --output DisplayPort-1-5 --off --output HDMI-A-1-3 --mode 1920x1080 --pos 1920x0 --rotate normal
