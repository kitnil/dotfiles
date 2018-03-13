#!/bin/sh
# Send a command to Emacs server.

set -e -x

nohup emacsclient -c -e "(wi-shell-send-command \"$@\")" &>/dev/null &
