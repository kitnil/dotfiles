FROM archlinux

RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S alacritty less seatd sway wmenu xorg-xwayland \
    && ln -s /usr/sbin/alacritty /usr/sbin/foot

RUN pacman --noconfirm -S sudo strace

RUN systemctl enable seatd

RUN useradd -m oleg \
    && passwd --delete oleg \
    && gpasswd -a oleg seat \
    && gpasswd -a oleg video \
    && gpasswd -a oleg wheel \
    && gpasswd -a oleg kmem

RUN sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

COPY rootfs/bin/entrypoint /bin/entrypoint
COPY rootfs/bin/run-sway /bin/run-sway
COPY rootfs/bin/dot_bashrc /home/oleg/.bashrc

LABEL "run.cluster.local"="docker run --dns 8.8.8.8 --detach --cap-add SYS_ADMIN --cap-add SYS_TTY_CONFIG --cap-add SYS_NICE --cap-add NET_BIND_SERVICE --cap-add SYS_TIME --device /dev/dri --device /dev/tty5 --name archlinux-1 --env container=docker --tmpfs /tmp --tmpfs /run --stop-signal SIGRTMIN+3 --device /dev/input archlinux-systemd"

ENTRYPOINT ["/bin/entrypoint"]
