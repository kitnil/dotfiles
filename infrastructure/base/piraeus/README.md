# Installation

[piraeus-operator/security.md at master · piraeusdatastore/piraeus-operator](https://github.com/piraeusdatastore/piraeus-operator/blob/master/doc/security.md)

> If you ever delete or change the passphrase secret, the LINSTOR Controller
> can no longer start with a failure message like Automatic injection of
> passphrase failed. You can force the Controller to start by setting the
> luksSecret value in the LinstorController resource to "". This will not give
> you access to encrypted items such as remotes, but it will allow the
> Controller to start. If you need to recover encrypted values, you need to
> restore the original secret.

``` shell
cat <<EOF
apiVersion: v1
data:
  MASTER_PASSPHRASE: $(pass show majordomo/public/kubernetes/piraeus/piraeus-op-passphrase)
immutable: true
kind: Secret
metadata:
  name: piraeus-op-passphrase
  namespace: piraeus
type: Opaque
EOF
```

# Node configuration

``` shell
cfdisk /dev/sda
pvcreate /dev/sda2
vgcreate vg0 /dev/sda2
lvcreate -l 100%FREE -Zn --type thin-pool --thinpool pool0 vg0
```

Run nix-shell to get `kubectl linstor`.

```
$ kubectl linstor storage-pool create lvmthin kube4 pool0 lvm2/thinpool2
SUCCESS:
    Successfully set property key(s): StorDriver/StorPoolName
SUCCESS:
Description:
    New storage pool 'pool0' on node 'kube4' registered.
Details:
    Storage pool 'pool0' on node 'kube4' UUID is: aa0b3c74-fe6b-42ff-83f5-08a00aa28ef2
SUCCESS:
    (kube4) Changes applied to storage pool 'pool0'
```

# Test

``` yaml
kind: PersistentVolumeClaim
apiVersion: v1
metadata:
  name: dbench-linstor
spec:
  storageClassName: linstor-pool0
  accessModes:
    - ReadWriteMany
  resources:
    requests:
      storage: 5Gi
---
apiVersion: batch/v1
kind: Job
metadata:
  name: dbench-linstor
spec:
  template:
    spec:
      tolerations:
        - operator: Exists
      # nodeSelector:
      #   kubernetes.io/hostname: kube2
      containers:
      - name: dbench
        image: sotoaster/dbench:latest
        imagePullPolicy: IfNotPresent
        env:
          - name: DBENCH_MOUNTPOINT
            value: /data
          - name: FIO_SIZE
            value: 1G
        volumeMounts:
        - name: dbench-pv
          mountPath: /data
      restartPolicy: Never
      volumes:
      - name: dbench-pv
        persistentVolumeClaim:
          claimName: dbench-linstor
  backoffLimit: 4
```

# Debug

## Linstor fails to create PVC and tries to remove it after creation

Linstor/DRBD Grafana dashboards should not contain any bad (red) graphics.

Make sure there are no:

- Any failed Resource Definitions
- Any failed Storage Pools
- Any failed Resources

Also make sure there are no:

- Disconnected DRBD resources
- DRBD Resources Without Quorum
- DRBD Resources Out of Sync
- DRBD Resources with Storage Failure

# TODO

- [ ] DrbdOptions/AutoEvictAllowEviction setted from default value of 'true'
      to custom 'false' [1] to avoid eviction from nodes with networking
      failure.  The piraeus-op-cs-controller deployment restarted.  No
      disconnects so far, but we should do something other than disabling
      DrbdOptions/AutoEvictAllowEviction option.

      [1]:
      ```
      $ linstor controller list-properties
        DrbdOptions/AutoEvictAllowEviction false
      ```
