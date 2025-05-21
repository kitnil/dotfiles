FROM archlinux

COPY build.sh /usr/local/bin/build.sh
RUN /usr/local/bin/build.sh

COPY rootfs/bin/entrypoint /bin/entrypoint

ENTRYPOINT ["/bin/entrypoint"]
