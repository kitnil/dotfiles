#!/bin/sh
exec -a ffmpeg urxvt -name ffpeg -e ffmpeg -f x11grab -r 30 -video_size 1920x1080 -i $DISPLAY -c:v ffvhuff /tmp/screencast-$(date +%F-%H-%M).mkv $@
