FROM debian:12
RUN apt update \
    && apt install -y systemd alacritty less seatd sway sudo strace

RUN apt update \
    && apt install -y procps xwayland udev libinput-tools bash-completion

RUN apt update \
    && apt install -y firefox-esr

RUN apt update \
    && apt install -y wget \
    && wget -O /tmp/Yandex.deb https://iso.wugi.info/Yandex.deb \
    && dpkg -i /tmp/Yandex.deb \
    ; rm -f /tmp/Yandex.deb \
    && apt -y -f install

RUN apt update \
    && apt install -y tmux

RUN apt update \
    && apt install -y pulseaudio

RUN apt update \
    && apt install -y docker.io

RUN ln -s /usr/sbin/alacritty /usr/sbin/foot \
    && systemctl enable seatd \
    && systemctl enable getty@tty6.service

RUN useradd -m oleg \
    && passwd --delete oleg \
    && gpasswd -a oleg video \
    && gpasswd -a oleg sudo \
    && chsh --shell /bin/bash oleg

RUN sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

COPY rootfs/bin/entrypoint /bin/entrypoint
COPY rootfs/bin/run-sway /bin/run-sway
COPY rootfs/bin/dot_bashrc /home/oleg/.bashrc

STOPSIGNAL SIGRTMIN+3
CMD [ "/bin/entrypoint" ]
