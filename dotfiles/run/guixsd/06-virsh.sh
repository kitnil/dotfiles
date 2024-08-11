#!/bin/sh -e
sudo virsh nodedev-detach pci_0000_12_00_0
sudo virsh nodedev-detach pci_0000_12_00_1
sudo virsh start win10
