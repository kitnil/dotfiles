#!/bin/sh -e

mkdir -p /mnt/guix-workstation/tmp
mount -t tmpfs -o rw,relatime,size=100M,rshared none /mnt/guix-workstation/tmp

mkdir -p /mnt/guix-workstation/run
mount -t tmpfs -o rw,relatime,size=100M,rshared none /mnt/guix-workstation/run
