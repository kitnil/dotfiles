#!/usr/bin/env bash

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
    HTTPTunnelPort 0.0.0.0:9250
    Bridge $(pass show tor/bridge/1)
    Bridge $(pass show tor/bridge/2)
    Bridge $(pass show tor/bridge/3)
    Bridge $(pass show tor/bridge/4)
EOF
