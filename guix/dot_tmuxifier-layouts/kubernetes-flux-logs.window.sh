# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/kubernetes"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "logs-flux"

mapfile -t pod_names < <(kubectl get --no-headers=true -n flux-system -o custom-columns='NAME:metadata.name' pods)

run_cmd "kubectl -n flux-system logs --follow=true --tail=20 --all-containers=true ${pod_names[0]}"
split_v 50

run_cmd "kubectl -n flux-system logs --follow=true --tail=20 --all-containers=true ${pod_names[1]}"
split_v 50

run_cmd "kubectl -n flux-system logs --follow=true --tail=20 --all-containers=true ${pod_names[2]}"
split_v 50

run_cmd "kubectl -n flux-system logs --follow=true --tail=20 --all-containers=true ${pod_names[3]}"
