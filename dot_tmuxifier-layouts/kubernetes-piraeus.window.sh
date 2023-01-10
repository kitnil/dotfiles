# Set window root path. Default is `$session_root`.
# Must be called before `new_window`.
#window_root "~/Projects/kubernetes"

# Create new window. If no argument is given, window name will be based on
# layout file name.
new_window "piraeus"

run_cmd "kubectl watch piraeus"

mapfile -t pod_names < <(kubectl get --no-headers=true -n piraeus -o custom-columns='NAME:metadata.name,NAME:spec.nodeName' pods | awk '/piraeus-op-ns-node/ && (/kube1/ || /kube2/) { print $1 }')

split_v 50
run_cmd "kubectl -n piraeus exec -it ${pod_names[0]} -- /bin/bash"

split_h 50
run_cmd "kubectl -n piraeus exec -it ${pod_names[1]} -- /bin/bash"


