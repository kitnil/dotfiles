#!/bin/sh
GUIX_LATEST=/root/.config/guix/latest
unlink $GUIX_LATEST && ln -s $(readlink /home/natsu/.config/guix/latest) $GUIX_LATEST
