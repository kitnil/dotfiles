#!/bin/sh

# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
window_root "$HOME"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "health"

# Run commands.
run_cmd 'viddy --interval 10s --max-history 50 --differences curl --silent -XGET http://172.16.103.101:9200/_cluster/health?pretty'

split_v 50
run_cmd "viddy --interval 30s --max-history 50 curl --silent -XGET 'http://es.intr:9200/_cat/allocation?v'"
