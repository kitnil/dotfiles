#!/bin/sh
GUIX_LATEST=/root/.config/guix/latest
unlink $GUIX_LATEST && ln -s $(readlink $HOME/.config/guix/latest) $GUIX_LATEST
