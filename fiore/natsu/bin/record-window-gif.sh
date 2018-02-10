#!/bin/sh

# Code from: https://ask.fedoraproject.org/en/question/10272/how-can-i-capture-a-video-of-my-screen-in-fedora/?answer=38332#post-id-38332

NAME=screencast-$(date +%Y%m%d%H%M)
FPS=5
THREADS=3

tmpfile=screengrab.tmp.$$
trap 'touch $tmpfile; rm -f $tmpfile' 0

xwininfo > $tmpfile 2>/dev/null
left=$(grep 'Absolute upper-left X:' $tmpfile | awk '{print $4}');
top=$(grep 'Absolute upper-left Y:' $tmpfile | awk '{print $4}');
width=$(grep 'Width:' $tmpfile | awk '{print $2}');
height=$(grep 'Height:' $tmpfile | awk '{print $2}');
geom="-geometry ${width}x${height}+${left}+${top}"
echo "Geometry: ${geom}"
size="${width}x${height}"
pos="${left},${top}"
echo "pos=$pos size=$size"

sleep 2

ffmpeg -y \
       -f x11grab \
       -r $FPS \
       -video_size $size \
       -i ${DISPLAY-0:0}+${pos} \
       -c:v ffvhuff \
       $NAME.mkv \
       $@

palette="palette.png"
filters="fps=$FPS,scale=0:-1:flags=lanczos"

ffmpeg -v warning -i $NAME.mkv -vf "$filters,palettegen" -threads $THREADS -y $palette
ffmpeg -v warning -i $NAME.mkv -i $palette -lavfi "$filters [x]; [x][1:v] paletteuse" -threads $THREADS -y $NAME.gif

rm -f $palette
