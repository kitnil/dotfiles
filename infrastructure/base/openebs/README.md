# Example

Expose Piraeus PVC via NFS as a ClusterIP service.

```
$ kubectl -n roundcube get pvc roundcube-attachments
NAME                    STATUS   VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS         AGE
roundcube-attachments   Bound    pvc-f490dd89-6c89-4787-9b9a-d96e2b7a5a72   1Gi        RWX            openebs-kernel-nfs   16m

$ kubectl -n openebs get pvc nfs-pvc-f490dd89-6c89-4787-9b9a-d96e2b7a5a72
NAME                                           STATUS   VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS    AGE
nfs-pvc-f490dd89-6c89-4787-9b9a-d96e2b7a5a72   Bound    pvc-c5bcfd80-c8d5-4bc7-9eba-640620208f0e   1Gi        RWO            linstor-pool1   17m

$ kubectl -n openebs get pod nfs-pvc-f490dd89-6c89-4787-9b9a-d96e2b7a5a72-78769bf776-p74vk -o wide
NAME                                                            READY   STATUS    RESTARTS   AGE   IP           NODE    NOMINATED NODE   READINESS GATES
nfs-pvc-f490dd89-6c89-4787-9b9a-d96e2b7a5a72-78769bf776-p74vk   1/1     Running   0          18m   10.1.3.123   kube4   <none>           <none>

$ kubectl -n openebs get svc nfs-pvc-f490dd89-6c89-4787-9b9a-d96e2b7a5a72
NAME                                           TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)            AGE
nfs-pvc-f490dd89-6c89-4787-9b9a-d96e2b7a5a72   ClusterIP   10.8.240.226   <none>        2049/TCP,111/TCP   18m
```
