``` shell
curl --request POST --silent 10.8.147.234:80

cat <<EOF | kubectl apply -f -
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
    Bridge $(curl --silent 10.8.147.234:80 | jq '.[0]' --raw-output)
    Bridge $(curl --silent 10.8.147.234:80 | jq '.[1]' --raw-output)
EOF
```
