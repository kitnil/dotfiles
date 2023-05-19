#!/bin/sh

# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
window_root "$HOME"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "curator"

# Run commands.
run_cmd "ssh -q fluentd.intr sudo tail -f /home/jenkins/es-curator/curator.log"
