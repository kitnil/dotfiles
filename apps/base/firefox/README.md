Generate secret for Xvnc

```
cat <<EOF | kubectl create -f -
apiVersion: v1
kind: Secret
type: Opaque
metadata:
  name: vnc
  namespace: windows
data:
  passwd: $(pass show kubernetes/firefox/vnc/passwd | vncpasswd -f > passwd; base64 < passwd)
EOF
```
