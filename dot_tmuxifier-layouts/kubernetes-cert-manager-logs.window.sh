# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/kubernetes"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "logs-cert-manager"

mapfile -t pod_names < <(kubectl get --no-headers=true -n cert-manager -o custom-columns='NAME:metadata.name' pods)

run_cmd "viddy --no-title --interval 10s --differences kubectl get certificates --all-namespaces=true"
split_v 50

run_cmd "kubectl -n cert-manager logs --follow=true --tail=20 --all-containers=true ${pod_names[0]}"
split_v 50

run_cmd "kubectl -n cert-manager logs --follow=true --tail=20 --all-containers=true ${pod_names[1]}"
split_v 50

run_cmd "kubectl -n cert-manager logs --follow=true --tail=20 --all-containers=true ${pod_names[2]}"
