#!/bin/sh

vncviewer -AutoSelect=0 -PreferredEncoding=Raw -FullColor=1 -NoJPEG=1 -CompressLevel=0 -passwd=$HOME/.vnc/passwd 127.0.0.1:5901
