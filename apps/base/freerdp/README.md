Generate secret for Xvnc

```
cat <<EOF | kubectl create -n freerdp -f -
apiVersion: v1
kind: Secret
type: Opaque
metadata:
  name: vnc
data:
  passwd: $(pass show kubernetes/firefox/vnc/passwd | vncpasswd -f > passwd; base64 < passwd)
EOF
```
