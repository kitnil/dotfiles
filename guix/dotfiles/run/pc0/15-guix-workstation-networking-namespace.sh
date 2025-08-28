ip netns add guix-workstation
ip link add name guix0 type veth peer name guix1
ip link set dev guix1 netns guix-workstation
ip netns exec guix-workstation ip link set guix1 name eth0
ip netns exec guix-workstation ip link set eth0 up
ip link set guix0 master br0
ip link set guix0 up
ip netns exec guix-workstation ip addr add 192.168.0.194/24 dev eth0
ip netns exec guix-workstation ip route add default via 192.168.0.1
