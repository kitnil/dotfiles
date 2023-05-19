#!/bin/sh

# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
window_root "$HOME"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "shards"

# Run commands.
run_cmd "viddy --no-title --interval 10m --max-history 10 elasticsearch shards"
