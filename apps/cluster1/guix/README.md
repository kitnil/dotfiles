```
cat <<EOF | kubectl create -n guix -f -
kind: Secret
apiVersion: v1
metadata:
  name: ssh-secret
type: kubernetes.io/ssh-auth
stringData:
  user: user
  disable-strict-host-key-checking: "true"
  ssh-privatekey: |
$(cat "$HOME/.ssh/id_vm-guix-datavolume" | sed 's/^/    /')
EOF
```
