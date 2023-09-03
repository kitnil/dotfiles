#!/run/current-system/profile/bin/bash

if [[ -e /sys/kernel/debug/dri/0/amdgpu_gpu_recover ]]
then
    if [[ -e /sys/bus/pci/devices/0000:12:00.1/remove ]]
    then
        echo 1 > /sys/bus/pci/devices/0000:12:00.1/remove
        /run/current-system/profile/bin/cat /sys/kernel/debug/dri/0/amdgpu_gpu_recover
        echo 1 > /sys/bus/pci/rescan
    fi
    /run/current-system/profile/bin/cat /sys/kernel/debug/dri/0/amdgpu_gpu_recover
elif [[ -e /sys/kernel/debug/dri/2/amdgpu_gpu_recover ]]
then
    if [[ -e /sys/bus/pci/devices/0000:12:00.1/remove ]]
    then
        echo 1 > /sys/bus/pci/devices/0000:12:00.1/remove
        /run/current-system/profile/bin/cat /sys/kernel/debug/dri/2/amdgpu_gpu_recover
        echo 1 > /sys/bus/pci/rescan
    fi
    /run/current-system/profile/bin/cat /sys/kernel/debug/dri/2/amdgpu_gpu_recover
fi
