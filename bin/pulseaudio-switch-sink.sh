#!/bin/sh

# Switch Pulseaudio sink between two of them.

default_sink=$(ponymix defaults | awk '/^sink/ {gsub(":",""); print $2}');

function set_default_sink
{
    ponymix set-default --device $1
}

case $default_sink in
    1) set_default_sink 0;;
    0) set_default_sink 1;;
esac
