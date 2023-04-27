# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/kubernetes"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "pods"

run_cmd "viddy --interval 10 kubectl get pods --field-selector status.phase=Pending --all-namespaces -o wide"
