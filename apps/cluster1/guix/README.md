```
cat <<EOF | kubectl create -n guix -f -
apiVersion: v1
kind: Secret
metadata:
  name: ssh-secrets
type: Opaque
data:
  ssh_host_rsa_key: $(cat ~/.ssh/id_vm-guix-datavolume | base64 | tr -d '\n')
EOF
```
