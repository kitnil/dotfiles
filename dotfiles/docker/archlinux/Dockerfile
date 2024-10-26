FROM archlinux

RUN pacman --noconfirm -Syu \
 && pacman --noconfirm -S less wmenu xorg-xwayland sudo strace tmux bash-completion fuse-overlayfs unzip

RUN useradd -m oleg \
 && passwd --delete oleg \
 && gpasswd -a oleg video \
 && gpasswd -a oleg wheel \
 && gpasswd -a oleg kmem

RUN sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

COPY rootfs/bin/entrypoint /bin/entrypoint

ADD https://github.com/steveseguin/social_stream/releases/download/0.1.52/socialstream-linux-AppImage.zip /tmp/socialstream-linux-AppImage.zip
RUN cd /usr/local/bin \
 && unzip /tmp/socialstream-linux-AppImage.zip \
 && rm /tmp/socialstream-linux-AppImage.zip \
 && mv socialstream-0.1.52-x86_64.AppImage socialstream \
 && chmod 555 socialstream

ENTRYPOINT ["/bin/entrypoint"]
