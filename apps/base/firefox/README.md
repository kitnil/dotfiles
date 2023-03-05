Generate secret for Xvnc

```
cat <<EOF | kubectl create -f -
apiVersion: v1
kind: Secret
type: Opaque
metadata:
  name: vnc
  namespace: firefox
data:
  passwd: $(pass show kubernetes/firefox/vnc/passwd | guix shell tigervnc-server -- vncpasswd -f > passwd; base64 < passwd)
EOF
```
