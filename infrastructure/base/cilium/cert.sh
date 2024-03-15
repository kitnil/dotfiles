#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

cat > cilium-secret-values.yaml <<EOF
tls:
  ca:
    cert: $(cat ${PKI_PREFIX:-}ca.pem | base64 -w0)
    key: $(cat ${PKI_PREFIX:-}ca-key.pem | base64 -w0)

clustermesh:
  name: "${CLUSTER_NAME:-cluster2}"
  useAPIServer: true # Generate cilium-clustermesh secret
  apiserver:
    tls:
      auto:
        enabled: false # Use pregenerated secrets
      ca:
        cert: $(cat ${PKI_PREFIX:-}ca.pem | base64 -w0)
        key: $(cat ${PKI_PREFIX:-}ca-key.pem | base64 -w0)
      admin:
        cert: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-admin-cert.pem | base64 -w0)
        key: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-admin-cert-key.pem | base64 -w0)
      client:
        cert: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-client-cert.pem | base64 -w0)
        key: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-client-cert-key.pem | base64 -w0)
      remote:
        cert: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-remote-cert.pem | base64 -w0)
        key: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-remote-cert-key.pem | base64 -w0)
      server:
        cert: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-server-cert.pem | base64 -w0)
        key: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-server-cert-key.pem | base64 -w0)
  config:
    enabled: true # Generate cilium-clustermesh secret
    clusters:
    - name: ${REMOTE_CLUSTER_NAME:-cluster1}
      address: ${REMOTE_CLUSTER_ADDRESS:-192.168.25.2}
      port: ${REMOTE_CLUSTER_PORT:-32379}
      tls:
        cert: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-remote-cert.pem | base64 -w0)
        key: $(cat ${PKI_PREFIX:-}clustermesh-apiserver-remote-cert-key.pem | base64 -w0)
EOF

cat > secret-cilium-secret-values.yaml <<EOF
apiVersion: v1
kind: Secret
metadata:
  name: cilium-secret-values
  namespace: kube-system
data:
  values.yaml: $(cat cilium-secret-values.yaml | base64 -w0)
EOF

