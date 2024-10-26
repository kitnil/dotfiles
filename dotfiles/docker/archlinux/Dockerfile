FROM archlinux

RUN pacman --noconfirm -Syu \
 && pacman --noconfirm -S less wmenu xorg-xwayland sudo strace tmux bash-completion fuse-overlayfs

RUN useradd -m oleg \
 && passwd --delete oleg \
 && gpasswd -a oleg video \
 && gpasswd -a oleg wheel \
 && gpasswd -a oleg kmem

RUN sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

COPY rootfs/bin/entrypoint /bin/entrypoint

ENTRYPOINT ["/bin/entrypoint"]
