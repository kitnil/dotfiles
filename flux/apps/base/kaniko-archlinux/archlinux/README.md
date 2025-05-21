sudo chown oleg: /run/user/1000
export WAYLAND_DISPLAY=wayland-1
export XDG_RUNTIME_DIR=/run/user/1000
export DISPLAY=:0

alacritty
