FROM docker.io/kalilinux/kali-rolling

COPY build.sh /usr/local/bin/build.sh
RUN /usr/local/bin/build.sh

COPY rootfs/bin/entrypoint /bin/entrypoint

STOPSIGNAL SIGRTMIN+3
ENTRYPOINT ["/bin/entrypoint"]
