FROM archlinux

RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S alacritty less seatd sway wmenu xorg-xwayland \
    && ln -s /usr/sbin/alacritty /usr/sbin/foot

RUN pacman --noconfirm -S sudo

RUN systemctl enable seatd

RUN useradd -m oleg \
    && passwd --delete oleg \
    && gpasswd -a oleg seat \
    && gpasswd -a oleg video \
    && gpasswd -a oleg wheel

RUN sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

COPY rootfs/bin/entrypoint /bin/entrypoint

LABEL "run.cluster.local"="docker run --dns 8.8.8.8 --detach --privileged --device /dev/dri --device /dev/tty5 --name archlinux-1 --env container=docker --tmpfs /tmp --tmpfs /run --stop-signal SIGRTMIN+3 archlinux-systemd"

ENTRYPOINT ["/bin/entrypoint"]
