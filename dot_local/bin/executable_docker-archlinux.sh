#!/usr/bin/env bash
sudo mkdir /sys/fs/cgroup/systemd
sudo mount -t cgroup cgroup -o none,name=systemd /sys/fs/cgroup/systemd
docker run --cgroupns=host --privileged --name archlinux --env container=docker -t -v /sys/fs/cgroup:/sys/fs/cgroup --tmpfs /tmp --tmpfs /run --entrypoint /sbin/init --stop-signal SIGRTMIN+3 --rm archlinux
