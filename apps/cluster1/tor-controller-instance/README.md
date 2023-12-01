``` shell
cat <<EOF | kubectl create -f -
apiVersion: v1
kind: ConfigMap
metadata:
  name: tor-configuration
  namespace: tor-controller-instance
data:
  torrc: |
    ExitNodes {nl},{fr},{de}
    UseBridges 1
    ClientTransportPlugin obfs4 exec /usr/local/bin/obfs4proxy
    Bridge $(pass show tor/bridge/1)
    Bridge $(pass show tor/bridge/2)
EOF
```
