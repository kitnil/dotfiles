#!/usr/bin/env bash

set -o nounset -o errexit -o pipefail

cat > "cilium-ca.yaml" <<EOF
apiVersion: v1
data:
  ca.crt: $(cat /etc/kubernetes/pki/ca.pem | base64 -w0)
  ca.key: $(cat /etc/kubernetes/pki/ca-key.pem | base64 -w0)
kind: Secret
metadata:
  name: cilium-ca
  namespace: kube-system
type: Opaque
EOF

cat > "clustermesh-apiserver-admin-cert.yaml" <<EOF
apiVersion: v1
data:
  ca.crt: $(cat /etc/kubernetes/pki/ca.pem | base64 -w0)
  tls.crt: $(cat /etc/kubernetes/pki/clustermesh-apiserver-admin-cert.pem | base64 -w0)
  tls.key: $(cat /etc/kubernetes/pki/clustermesh-apiserver-admin-cert-key.pem | base64 -w0)
kind: Secret
metadata:
  name: clustermesh-apiserver-admin-cert
  namespace: kube-system
type: kubernetes.io/tls
EOF

cat > "clustermesh-apiserver-client-cert.yaml" <<EOF
apiVersion: v1
data:
  ca.crt: $(cat /etc/kubernetes/pki/ca.pem | base64 -w0)
  tls.crt: $(cat /etc/kubernetes/pki/clustermesh-apiserver-client-cert.pem | base64 -w0)
  tls.key: $(cat /etc/kubernetes/pki/clustermesh-apiserver-client-cert-key.pem | base64 -w0)
kind: Secret
metadata:
  name: clustermesh-apiserver-client-cert
  namespace: kube-system
type: kubernetes.io/tls
EOF

cat > "clustermesh-apiserver-remote-cert.yaml" <<EOF
apiVersion: v1
data:
  ca.crt: $(cat /etc/kubernetes/pki/ca.pem | base64 -w0)
  tls.crt: $(cat /etc/kubernetes/pki/clustermesh-apiserver-remote-cert.pem | base64 -w0)
  tls.key: $(cat /etc/kubernetes/pki/clustermesh-apiserver-remote-cert-key.pem | base64 -w0)
kind: Secret
metadata:
  name: clustermesh-apiserver-remote-cert
  namespace: kube-system
type: kubernetes.io/tls
EOF

cat > "clustermesh-apiserver-server-cert.yaml" <<EOF
apiVersion: v1
data:
  ca.crt: $(cat /etc/kubernetes/pki/ca.pem | base64 -w0)
  tls.crt: $(cat /etc/kubernetes/pki/clustermesh-apiserver-server-cert.pem | base64 -w0)
  tls.key: $(cat /etc/kubernetes/pki/clustermesh-apiserver-server-cert-key.pem | base64 -w0)
kind: Secret
metadata:
  name: clustermesh-apiserver-server-cert
  namespace: kube-system
type: kubernetes.io/tls
EOF

cat > kustomization.yaml <<EOF
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
namespace: kube-system
resources:
- cilium-ca.yaml
- clustermesh-apiserver-admin-cert.yaml
- clustermesh-apiserver-client-cert.yaml
- clustermesh-apiserver-remote-cert.yaml
- clustermesh-apiserver-server-cert.yaml
EOF


