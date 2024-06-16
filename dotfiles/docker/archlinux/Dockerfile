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

# Install yay
RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S --needed git base-devel \
    && git clone https://aur.archlinux.org/yay-bin.git /usr/local/src/yay-bin \
    && cd /usr/local/src/yay-bin
RUN chown -R oleg: /usr/local/src/yay-bin
RUN sudo -u oleg bash -c 'cd /usr/local/src/yay-bin || exit 1; makepkg --noconfirm -si'

# Install idea
RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S libxtst \
    && yay --noconfirm -S intellij-idea-community-edition
COPY rootfs/bin/run-idea /bin/run-idea

RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S bash-completion \
    && pacman --noconfirm -S fuse-overlayfs \
    && pacman --noconfirm -S docker

LABEL "run.cluster.local"="docker run --dns 8.8.8.8 --detach --cap-add SYS_ADMIN --cap-add SYS_TTY_CONFIG --cap-add SYS_NICE --cap-add NET_BIND_SERVICE --cap-add SYS_TIME --cap-add NET_RAW --cap-add NET_ADMIN --device /dev/dri --device /dev/tty5 --name archlinux-1 --env container=docker --tmpfs /tmp --tmpfs /run --stop-signal SIGRTMIN+3 --device /dev/input archlinux-systemd"

RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S tmux

RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S firefox

RUN pacman --noconfirm -Syu \
    && pacman --noconfirm -S pulseaudio

COPY rootfs/bin/run-pulseaudio /bin/run-pulseaudio

ENTRYPOINT ["/bin/entrypoint"]
