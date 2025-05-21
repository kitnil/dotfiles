#!/usr/bin/env bash

set -ex

ssh root@192.168.0.91 rm -rf /etc/kubernetes/pki
rsync -a /home/oleg/src/gitlab.intr/nixos/kubernetes-kube1-home/ssl/ root@192.168.0.91:/etc/kubernetes/pki/
ssh root@192.168.0.91 <<'EOF'
chown -R root: /etc/kubernetes/pki
systemctl stop kubelet
(set -e; pgrep -fa shim | awk '{ print $1 }' | xargs kill)
systemctl start kubelet
EOF

sudo rm -rf /etc/kubernetes/pki
sudo rsync -a /home/oleg/src/gitlab.intr/nixos/kubernetes-kube1-home/ssl/ /etc/kubernetes/pki/
sudo chown -R root: /etc/kubernetes/pki
sudo herd stop kubelet
sudo rm /var/lib/kubelet/.maintenance
sudo herd start kubelet

ssh root@192.168.0.192 rm -rf /etc/kubernetes/pki
rsync -a /home/oleg/src/gitlab.intr/nixos/kubernetes-kube1-home/ssl/ root@192.168.0.192:/etc/kubernetes/pki/
ssh root@192.168.0.192 <<'EOF'
chown -R root: /etc/kubernetes/pki
herd stop kubelet
rm /var/lib/kubelet/.maintenance
herd start kubelet
EOF
