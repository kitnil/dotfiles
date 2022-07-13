#!/bin/sh

current_date="$(date +'%Y-%m-%d %H:%M:%S')"
current_battery="$(cat /sys/class/power_supply/BAT1/capacity)"
printf "battery: %s    date: %s" "$current_battery" "$current_date"
