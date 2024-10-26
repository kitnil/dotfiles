FROM archlinux

RUN pacman --noconfirm -Syu \
 && pacman --noconfirm -S alsa-lib atk bash-completion cups fuse-overlayfs fuse2 gdk-pixbuf2 gtk3 less noto-fonts noto-fonts-cjk noto-fonts-emoji noto-fonts-extra nss strace sudo tmux unzip wmenu xorg-xwayland

RUN useradd -m oleg \
 && passwd --delete oleg \
 && gpasswd -a oleg video \
 && gpasswd -a oleg wheel \
 && gpasswd -a oleg kmem

RUN sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

COPY rootfs/bin/entrypoint /bin/entrypoint

ADD https://iso.wugi.info/socialstream-0.1.52-linux-AppImage.zip /tmp/socialstream-linux-AppImage.zip
RUN cd /usr/local/bin \
 && unzip /tmp/socialstream-linux-AppImage.zip \
 && rm /tmp/socialstream-linux-AppImage.zip \
 && mv socialstream-0.1.52-x86_64.AppImage socialstream \
 && chmod 555 socialstream

ENTRYPOINT ["/bin/entrypoint"]
