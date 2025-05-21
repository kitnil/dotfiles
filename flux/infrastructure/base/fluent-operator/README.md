```
cat | kubectl create -f - <<EOF
apiVersion: v1
data:
  username: $(printf admin | base64)
  password: $(pass show localhost/opensearch/admin | tr -d '\n' | base64)
kind: Secret
metadata:
  name: opensearch-http-authentication
  namespace: fluent-operator
type: Opaque
EOF
```
