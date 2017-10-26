#!/bin/sh

PROGRAM=htop

exec xterm -name $PROGRAM -e $PROGRAM "$@"
