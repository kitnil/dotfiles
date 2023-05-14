# Pools

## pool0

Very bad storage.  Do not use it for production workload if losing data is not
an option.

1. Old Linux kernel 4.4.
2. Deprecated Adaptec RAID controller.  A disk from this controller bypassed
   to a virtualmachine.

## pool1

Good storage.  Use for any workload which does not require much data.

1. Newest Linux kernel.
2. Software RAID 1 on each node.

## pool2

Better than pool0.  Primary used for customers Kubevirt workload.

1. New Linux kernel.
2. Deprecated Adaptec RAID controller.  Direct access from operating system.

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

``` shell
$ kubectl linstor storage-pool create lvmthin kube5005 pool0 vg0/pool0
SUCCESS:
    Successfully set property key(s): StorDriver/StorPoolName
SUCCESS:
Description:
    New storage pool 'pool0' on node 'kube5005' registered.
Details:
    Storage pool 'pool0' on node 'kube5005' UUID is: 8138f579-d57d-4e12-90a5-1df840e5fab9
SUCCESS:
    (kube5005) Changes applied to storage pool 'pool0'
```

```
$ kubectl linstor storage-pool create lvmthin kube5006 pool0 vg0/pool0
SUCCESS:
    Successfully set property key(s): StorDriver/StorPoolName
SUCCESS:
Description:
    New storage pool 'pool0' on node 'kube5006' registered.
Details:
    Storage pool 'pool0' on node 'kube5006' UUID is: 656511d4-7965-4ba0-9f3d-4a0ee8dae397
SUCCESS:
    (kube5006) Changes applied to storage pool 'pool0'
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
- [ ] Remove resource pvc-6a376a8e-1f55-437a-95c9-770a9bd1917c_00000.

- [ ] Remove resource pvc-39fd13a3-c95a-4fb0-baf4-5ac79854f787.
