#!/usr/bin/env bash

exec /gnu/store/3dwzy5gwagj0g838gr4kg77a0s0v1q0s-runc-1.1.12/sbin/runc exec -e TERM=screen-256color -t guix-builder /bin/sh -l
