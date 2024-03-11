#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail -o xtrace

if [[ -e /dev/mapper/crypt-nvme0n1 ]]
then
    :
else
    pass show luks2/luks2-header-210582390001289540AC \
        | sudo cryptsetup open \
               --allow-discards \
               --header="${HOME}/crypt/luks2-210582390001289540AC" \
               /dev/nvme0n1 \
               crypt-nvme0n1 \
               -
fi

subvols=(
    "archive"
    "phone"
    "src"
    "Maildir"
)
for subvol in "${subvols[@]}"
do
    location="${HOME}/${subvol}"
    if mountpoint -q "$location"
    then
        :
    else
        sudo mount "$location"
    fi
done

if [[ -e /dev/lvm2/swap ]]
then
    :
else
    sudo lvchange -ay /dev/lvm2/swap
    sudo swapon /dev/lvm2/swap
fi

if [[ -e /dev/mapper/crypt-srv ]]
then
    :
else
    pass show luks2/luks2-header-wd-wd181purp-85b6hy0 \
        | sudo cryptsetup open \
               --header="${HOME}/crypt/luks2-wd181purp-85b6hy0" \
               /dev/sdb \
               crypt-srv \
               -
fi

if mountpoint -q /srv
then
    :
else
    sudo mount /srv
fi

if [[ -e /dev/lvm2/qbittorrent-incomplete ]]
then
    :
else
    sudo lvchange -ay /dev/lvm2/qbittorrent-incomplete
fi

if mountpoint -q /mnt/qbittorrent-incomplete
then
    :
else
    sudo mount /dev/lvm2/qbittorrent-incomplete /mnt/qbittorrent-incomplete
fi

# for kubelet
if [[ -d /var/hpvolumes ]]
then
    if mountpoint --quiet /var/hpvolumes
    then
        :
    else
        # Uncomment if '/var/hpvolumes' directory is on a '/' file-system.
        # mount --bind /var/hpvolumes /var/hpvolumes
        sudo mount /var/hpvolumes

        sudo mount --make-shared /var/hpvolumes
    fi
fi
