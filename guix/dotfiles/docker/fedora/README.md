docker build -t docker-registry.wugi.info/os/fedora:34 .

mkdir -p /sys/fs/cgroup/systemd
mount -t cgroup -o none,name=systemd cgroup /sys/fs/cgroup/systemd

docker run -it --tmpfs /tmp --tmpfs /run -v /sys/fs/cgroup:/sys/fs/cgroup:ro --name fedora docker-registry.wugi.info/os/fedora:34

https://developers.redhat.com/blog/2016/09/13/running-systemd-in-a-non-privileged-container#the_quest
