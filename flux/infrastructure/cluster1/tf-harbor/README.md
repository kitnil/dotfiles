``` shell
cat | kubectl apply -f - <<EOF
apiVersion: v1
stringData:
  HARBOR_PASSWORD: $(pass show harbor.home.wugi.info/admin)
  HARBOR_URL: https://harbor.home.wugi.info/
  HARBOR_USERNAME: admin
  KUBERNETES_USER: kubernetes
  KUBERNETES_PASSWORD: $(pass show harbor.home.wugi.info/kubernetes)
kind: Secret
metadata:
  name: tf-harbor-secrets
  namespace: flux-system
type: Opaque
EOF
```
