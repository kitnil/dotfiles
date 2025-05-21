FROM ubuntu:20.04
ADD build.sh /build.sh
RUN /build.sh
USER user
ENTRYPOINT [ "paws-receive" ]
