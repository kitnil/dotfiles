``` shell
cat <<EOF | kubectl create -f -
apiVersion: v1
data:
  password: |
    $(pass show harbor.home.wugi.info/kubernetes | base64 | tr -d '\n')
kind: Secret
metadata:
  name: docker-credentials
  namespace: skopeo
type: Opaque
EOF
```
