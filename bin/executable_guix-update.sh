#!/bin/sh
GUIX_LATEST_USER=/home/natsu/.config/guix/latest
GUIX_LATEST_ROOT=/root/.config/guix/latest

# Echo evalueated commands
set -x

# Link to root guix from user guix
unlink $GUIX_LATEST_ROOT\
    && ln -s $(readlink $GUIX_LATEST_USER) $GUIX_LATEST_ROOT

test $(readlink $GUIX_LATEST_USER) == $(readlink $GUIX_LATEST_ROOT)
