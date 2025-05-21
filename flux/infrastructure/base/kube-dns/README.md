# Initialization

For the new cluster you should install kube-dns by hands:

``` shell
kubectl kustomize . | kubectl create -f -
```
