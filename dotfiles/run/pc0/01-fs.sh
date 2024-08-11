#!/bin/sh -e
sudo mount /dev/vg0/data1 /mnt
sudo mount --bind /mnt/home/oleg/.var/app/ru.linux_gaming.PortProton /home/oleg/.var/app/ru.linux_gaming.PortProton
sudo mount --bind /mnt/home/oleg/.local/share/guix-sandbox-home/.local/share/Steam /home/oleg/.local/share/guix-sandbox-home/.local/share/Steam
sudo umount /mnt
