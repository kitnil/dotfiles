#!/bin/sh
(setxkbmap -query | grep -q "layout:\s\+us") && setxkbmap ru || setxkbmap us && xmodmap ~/.Xmodmap
