``` shell
cat | kubectl create -f - <<EOF
apiVersion: v1
stringData:
  HARBOR_PASSWORD: $(pass show harbor.wugi.info/admin)
  HARBOR_URL: https://harbor.home.wugi.info/
  HARBOR_USERNAME: admin
kind: Secret
metadata:
  name: tf-harbor-secrets
  namespace: flux-system
type: Opaque
EOF
```
