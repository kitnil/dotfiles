#!/bin/sh
exec mpv $(xclip -o -selection clipboard)
