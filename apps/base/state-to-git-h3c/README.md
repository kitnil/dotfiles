```
cat | kubectl create -f - <<EOF
apiVersion: v1
data:
  h3c: $(pass show majordomo/private/ssh/router | base64)
  juniper: $(pass show majordomo/private/ssh/router | base64)
  cisco: $(pass show majordomo/private/ssh/router | base64)
  support: $(pass show majordomo/public/ssh/switch | base64)
  ansible: $(pass show majordomo/public/majordomo/ansible-majordomo-history/passwords | base64)
kind: Secret
metadata:
  name: ssh-password
  namespace: state-to-git
type: Opaque
EOF
```
