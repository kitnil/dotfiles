# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "backup"

run_cmd "backup mount /mnt/backup"

# Split window into panes.
split_v 50

# Run commands.
run_cmd "sudo -i"
