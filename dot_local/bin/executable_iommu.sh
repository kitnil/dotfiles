#!/usr/bin/env bash

(
    shopt -s nullglob
    while IFS= read -r -d '' group
    do
        printf "IOMMU Group %s:\n" "${group##*/}"
        for d in "$group"/devices/*; do
            printf "\t%s\n" "$(lspci -nns "${d##*/}")"
        done;
    done < <(find /sys/kernel/iommu_groups/* -maxdepth 0 -type d | sort -V)
)

echo -e "\n\nReset support:"
(
    while IFS= read -r -d '' iommu_group
    do
        echo "IOMMU group $(basename "$iommu_group")"
        for device in "$iommu_group"/devices/*
        do
            if [[ -e "$iommu_group"/devices/"$device"/reset ]]
            then
                echo -n "[RESET]"
            fi
            echo -n $'\t'
            lspci -nns "$device"
        done
    done < <(find /sys/kernel/iommu_groups/ -maxdepth 1 -mindepth 1 -type d)
)

echo -e "\n\nUSB controller:"
(
    for usb_ctrl in /sys/bus/pci/devices/*/usb*
    do
        pci_path=${usb_ctrl%/*}
        iommu_group="$(readlink "$pci_path"/iommu_group)"
        printf "Bus %s --> %s (IOMMU group %s)" "$(cat "$usb_ctrl"/busnum)" "${pci_path##*/}" "${iommu_group##*/}\n"
        lsusb -s "${usb_ctrl#*/usb}":
        echo
    done
)
