```
cat | kubectl create -f - <<EOF
apiVersion: v1
data:
  h3c: $(pass show majordomo/private/ssh/router | base64)
kind: Secret
metadata:
  name: ssh-password
  namespace: state-to-git
type: Opaque
EOF
```
