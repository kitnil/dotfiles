FROM archlinux

COPY build.sh /usr/local/bin/build.sh
COPY rootfs/bin/entrypoint /bin/entrypoint

RUN /usr/local/bin/build.sh

ENTRYPOINT ["/bin/entrypoint"]
