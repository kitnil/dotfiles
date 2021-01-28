#!/usr/bin/env bash

(
    shopt -s nullglob
    for g in `find /sys/kernel/iommu_groups/* -maxdepth 0 -type d | sort -V`; do
        echo "IOMMU Group ${g##*/}:"
        for d in $g/devices/*; do
            echo -e "\t$(lspci -nns ${d##*/})"
        done;
    done;
)

echo -e "\n\nReset support:"
(
    for iommu_group in $(find /sys/kernel/iommu_groups/ -maxdepth 1 -mindepth 1 -type d)
    do
        echo "IOMMU group $(basename "$iommu_group")"
        for device in $(\ls -1 "$iommu_group"/devices/)
        do
            if [[ -e "$iommu_group"/devices/"$device"/reset ]]
            then
                echo -n "[RESET]"
            fi
            echo -n $'\t'
            lspci -nns "$device"
        done
    done
)

echo -e "\n\nUSB controller:"
(
    for usb_ctrl in /sys/bus/pci/devices/*/usb*
    do
        pci_path=${usb_ctrl%/*}
        iommu_group=$(readlink $pci_path/iommu_group)
        echo "Bus $(cat $usb_ctrl/busnum) --> ${pci_path##*/} (IOMMU group ${iommu_group##*/})"
        lsusb -s ${usb_ctrl#*/usb}:
        echo
    done
)
