# Initialization

For the new cluster you should install Cilium by hands:

``` shell
helm install --version 1.12.4 --namespace kube-system --values values.yaml --repo https://helm.cilium.io/ cilium  cilium
```

# Troubleshooting

In case broken cilium the configuration could be modified with:
``` shell
kubectl edit -n kube-system configmaps cilium-config
```

after you should rollout all cilium resources, e.g.:

``` shell
kubectl -n kube-system rollout restart deployment cilium-operator
kubectl -n kube-system rollout status deployment cilium-operator

kubectl -n kube-system rollout restart daemonset cilium
kubectl -n kube-system rollout status daemonset cilium
```

## Reinstall Helm Chart

```
$ flux -n kube-system suspend helmrelease cilium
► suspending helmrelease cilium in kube-system namespace
✔ helmrelease suspended

$ flux -n kube-system resume helmrelease cilium
► resuming helmrelease cilium in kube-system namespace
✔ helmrelease resumed
◎ waiting for HelmRelease reconciliation
✔ HelmRelease reconciliation completed
✔ applied revision 1.12.4
```

# Multi cluster

[Deep Dive into Cilium Multi-cluster](https://cilium.io/blog/2019/03/12/clustermesh/)
