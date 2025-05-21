FROM docker.io/kalilinux/kali-rolling

COPY build.sh /usr/local/bin/build.sh
RUN /usr/local/bin/build.sh

COPY rootfs/bin/entrypoint /bin/entrypoint
COPY rootfs/home/oleg/.config/systemd/user/foot.service /home/oleg/.config/systemd/user/foot.service

STOPSIGNAL SIGRTMIN+3
ENTRYPOINT ["/bin/entrypoint"]
