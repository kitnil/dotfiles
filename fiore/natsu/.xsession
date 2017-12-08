#!/bin/sh

# Keyboard speed
# xset s 0
# xset dpms 0 0 1800

# Keyboard layout
# setxkbmap -layout us,ru -option grp:win_space_toggle

# Disable PC speaker
xset -b

# Disable accessiblity features
# xkbset -a

# Fix mouse wheel in gtk3
# https://github.com/stumpwm/stumpwm/wiki/FAQ
export GDK_CORE_DEVICE_EVENTS=1

# Use keyboard as mouse with <Shift+Num Lock>
# https://en.wikipedia.org/wiki/Mouse_keys
setxkbmap -option keypad:pointerkeys

# Enable X11 zap
setxkbmap -option terminate:ctrl_alt_bksp

# Wallpaper
# feh --bg-scale ~/Pictures/Wallpapers/current.png
xsetroot -solid white

# Rxvt colors and other
xrdb -merge ~/.Xresources

# xmodmap ~/.Xmodmap

export PATH=~/bin:$PATH

# Left hand mouse
# xmodmap ~/.Xmodmap

# exec stumpwm
# exec sbcl --eval "(ql:quickload :stumpwm)" --eval "(stumpwm:stumpwm)"
exec sbcl --load /home/natsu/bin/run-stumpwm
# exec /home/natsu/src/stumpwm/stumpwm
