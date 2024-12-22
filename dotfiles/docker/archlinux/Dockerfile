FROM archlinux

COPY build.sh /usr/local/bin/build.sh
RUN /usr/local/bin/build.sh

COPY rootfs/bin/entrypoint /bin/entrypoint
COPY --chown=oleg:users rootfs/home/oleg/.config/systemd/user/foot.service /home/oleg/.config/systemd/user/foot.service
COPY --chown=oleg:users rootfs/home/oleg/.config/systemd/user/idea.service /home/oleg/.config/systemd/user/idea.service

ENTRYPOINT ["/bin/entrypoint"]
