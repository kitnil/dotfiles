#!/bin/sh

# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
# window_root "/home/oleg/src/git.savannah.gnu.org/git/guix"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "machines"

# Split window into panes.
#split_v 20
#split_h 50

# Run commands.

split_v 50
run_cmd "ssh workstation.intr"
split_v 50
run_cmd "ssh guix.vm.wugi.info"
#run_cmd "top"     # runs in active pane
#run_cmd "date" 1  # runs in pane 1

# Paste text
#send_keys "top"    # paste into active pane
#send_keys "date" 1 # paste into pane 1

# Set active pane.
select_pane 0
