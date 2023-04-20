# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/kubernetes"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "cilium"

cilium_commands=(
    "cilium status"
    "cilium service list"
    "cilium endpoint list"
    "cilium bpf lb list"
)

mapfile -t pod_names < <(kubectl get --no-headers=true -n kube-system -o custom-columns='NAME:metadata.name' pods)

case "$KUBECONFIG" in
    "${HOME}/.kube/config-mjru-cluster1")
        context=mj-k8s-cluster0-lb
    ;;
    "${HOME}/.kube/config-mjru-cluster2")
        context=mj-k8s-cluster2-lb
    ;;
esac


for pod_name in "$pod_names"
do
    if [[ $pod_name == cilium-????? ]]
    then
        # run_cmd "viddy --no-title --interval 10s kubectl -n kube-system exec ${pod_name} -- cilium status"
        run_cmd "nix-shell --run 'viddy --no-title --interval 10s cilium status --context ${context}'"
        split_h 50
        run_cmd "viddy --no-title --interval 10s kubectl -n kube-system exec ${pod_name} -- cilium service list"
        split_v 50
        run_cmd "viddy --no-title --interval 10s kubectl -n kube-system exec ${pod_name} -- cilium endpoint list"
        select_pane 0
        split_v 50
        run_cmd "viddy --no-title --interval 10s kubectl -n kube-system exec ${pod_name} -- cilium bpf lb list"
        split_v 50
        run_cmd "nix-shell --run 'viddy --no-title --interval 10s cilium clustermesh status'"
    fi
done

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
