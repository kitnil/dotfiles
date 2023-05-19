# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/kubernetes"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "logs"

run_cmd "ssh -q kvm15.intr journalctl -u elasticsearch.service -f | grep -vF '[max_concurrent_shard_requests] is not supported in the metadata section and will be rejected in 7.x'"

split_v 50
run_cmd "ssh -q fluentd.intr sudo docker logs --tail 10 -f elk_elasticsearch_1 | grep -vF '[max_concurrent_shard_requests] is not supported in the metadata section and will be rejected in 7.x'"

split_v 50
run_cmd "ssh -q staff.intr journalctl -u elasticsearch.service -f | grep -vF '[max_concurrent_shard_requests] is not supported in the metadata section and will be rejected in 7.x'"

# Split window into panes.
#split_v 20
#split_h 50

# Run commands.
#run_cmd "top"     # runs in active pane
#run_cmd "date" 1  # runs in pane 1

# Paste text
#send_keys "top"    # paste into active pane
#send_keys "date" 1 # paste into pane 1

# Set active pane.
#select_pane 0
