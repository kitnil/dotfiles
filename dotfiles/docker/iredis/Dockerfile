FROM ubuntu:22.04

RUN apt-get update \
 && apt-get install --yes --no-install-recommends iredis \
 && rm -rf /var/lib/apt/lists/*

ENTRYPOINT [ "iredis" ]
