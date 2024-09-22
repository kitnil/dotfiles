#!/bin/sh

# This script:
# * launches a container with passing /dev/tty2;
# * able to run sway window manager
#
# requires deployed guix-image-workstation.scm container image.

set -o nounset -o errexit -o pipefail

cat > /home/user/entrypoint.sh <<'EOF'
#!/gnu/store/295aavfhzcn1vg9731zx9zw92msgby5a-bash-5.1.16/bin/sh

export PATH=/gnu/store/hbjc3f2mbi4wbbq78yr42ki9vkkpqi5h-util-linux-2.37.4/bin:$PATH

# Keyboard and mouse access are essential components for efficient operation
# within the Sway window manager environment. Running 'udevadm trigger' will
# grant access to the keyboard and mouse. You can verify their addition by
# using 'libinput list-devices'."
mount -o remount,rw /sys

umount /sys/fs/cgroup
# mount -t cgroup2 -o rw,relatime,nsdelegate,memory_recursiveprot cgroup2 /sys/fs/cgroup

# exec /gnu/store/295aavfhzcn1vg9731zx9zw92msgby5a-bash-5.1.16/bin/sh -l

exec /gnu/store/qai6vgzpdh9ca10ffbi9lpcmpnirl8yr-boot-program /gnu/store/43mknax9zzpx2q5k9w604fygb00511w1-system
EOF

docker_args=(
    --detach
    --device /dev/dri
    --device /dev/fuse
    --device /dev/input
    --device /dev/tty2
    --device /dev/tty2:/dev/tty0
    --entrypoint /bin/entrypoint.sh
    --name=guix
    --rm
    --tty
    --volume /etc/nsswitch.conf:/etc/nsswitch.conf:ro
    --volume /etc/services:/etc/services:ro
    --volume /home/user/entrypoint.sh:/bin/entrypoint.sh:ro
    --volume "/home/user/.local/share/chezmoi:/home/oleg/.local/share/chezmoi:ro"
)

capabilities=(
    SYS_ADMIN
)
for capability in "${capabilities[@]}"
do
    docker_args+=("--cap-add" "$capability")
done

sudo docker run "${docker_args[@]}" harbor.home.wugi.info/library/guix-image-workstation:latest
