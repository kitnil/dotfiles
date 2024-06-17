FROM ubuntu:24.04
RUN apt update \
    && apt install -y systemd alacritty less seatd sway sudo strace

RUN apt update \
    && apt install -y procps xwayland udev libinput-tools bash-completion

RUN apt update \
    && apt install -y tmux

RUN apt update \
    && apt install -y pulseaudio

RUN apt update \
    && apt install -y docker.io

ADD https://iso.wugi.info/ideaIC-2024.1.3.tar.gz /tmp
RUN tar -xzf /tmp/ideaIC-*.tar.gz -C /opt \
    && rm -f /tmp/ideaIC-*.tar.gz

RUN apt update \
    && apt install -y ubuntu-gnome-desktop

RUN ln -s /usr/sbin/alacritty /usr/sbin/foot \
    && systemctl enable seatd \
    && systemctl enable getty@tty8.service

RUN passwd --delete ubuntu \
    && gpasswd -a ubuntu render \
    && gpasswd -a ubuntu sudo \
    && chsh --shell /bin/bash ubuntu

RUN sed -i 's|# %wheel ALL=(ALL:ALL) NOPASSWD: ALL|%wheel ALL=(ALL:ALL) NOPASSWD: ALL|' /etc/sudoers

COPY rootfs/bin/entrypoint /bin/entrypoint
COPY rootfs/bin/run-sway /bin/run-sway
COPY rootfs/bin/dot_bashrc /home/oleg/.bashrc

STOPSIGNAL SIGRTMIN+3
CMD [ "/bin/entrypoint" ]
