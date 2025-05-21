FROM debian:12

RUN apt update \
 && apt install --yes dante-server libpam-pwdfile \
 && rm -rf /var/lib/apt/lists/*
