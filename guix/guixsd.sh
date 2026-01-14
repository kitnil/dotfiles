#!/usr/bin/env bash

cat /sys/class/drm/card0/device/power_dpm_force_performance_level
# low
echo 'manual' > /sys/class/drm/card0/device/power_dpm_force_performance_level
cat /sys/class/drm/card0/device/power_dpm_force_performance_level
# manual
echo "s 1 2200" > /sys/class/drm/card0/device/pp_od_clk_voltage
echo 'c' > /sys/class/drm/card0/device/pp_od_clk_voltage
