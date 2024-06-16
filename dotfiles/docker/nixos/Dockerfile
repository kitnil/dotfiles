FROM scratch
COPY nixos /
COPY rootfs/bin/entrypoint /bin/entrypoint
STOPSIGNAL SIGRTMIN+3
CMD [ "/bin/entrypoint" ]
