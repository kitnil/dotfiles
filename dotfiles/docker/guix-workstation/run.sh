#!/bin/sh

cat > /home/user/entrypoint.sh <<'EOF'
#!/gnu/store/295aavfhzcn1vg9731zx9zw92msgby5a-bash-5.1.16/bin/sh

PATH=/gnu/store/hbjc3f2mbi4wbbq78yr42ki9vkkpqi5h-util-linux-2.37.4/bin:/gnu/store/295aavfhzcn1vg9731zx9zw92msgby5a-bash-5.1.16/bin:$PATH
export PATH

# Keyboard and mouse access are essential components for efficient operation
# within the Sway window manager environment. Running 'udevadm trigger' will
# grant access to the keyboard and mouse. You can verify their addition by
# using 'libinput list-devices'."
mount -o remount,rw /sys

umount /sys/fs/cgroup
# mount -t cgroup2 -o rw,relatime,nsdelegate,memory_recursiveprot cgroup2 /sys/fs/cgroup

exec sh <<'EOL'
echo exec /gnu/store/{*boot-program,*system}
exec </dev/tty
EOL

# /gnu/store/qai6vgzpdh9ca10ffbi9lpcmpnirl8yr-boot-program /gnu/store/9y20rlzhdhzj3qc27vfwfrhafckbb6xp-system
# exec /gnu/store/qai6vgzpdh9ca10ffbi9lpcmpnirl8yr-boot-program /gnu/store/43mknax9zzpx2q5k9w604fygb00511w1-system
EOF

#    --entrypoint /bin/entrypoint.sh
docker_args=(
    --device /dev/dri
    --device /dev/fuse
    --device /dev/input
    --device /dev/tty2
    --device /dev/tty2:/dev/tty0
    --name=guix
    --tty
    --volume /etc/nsswitch.conf:/etc/nsswitch.conf:ro
    --volume /etc/services:/etc/services:ro
    --volume /home/user/entrypoint.sh:/bin/entrypoint.sh:ro
    --interactive
)
#    --volume "/home/user/.local/share/chezmoi:/home/oleg/.local/share/chezmoi:ro"

capabilities=(
    SYS_ADMIN
)
for capability in "${capabilities[@]}"
do
    docker_args+=("--cap-add" "$capability")
done

sudo docker run "${docker_args[@]}" harbor.home.wugi.info/library/guix-image-workstation:latest
