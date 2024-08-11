sudo ip link add macvlan0 link eth0 type macvlan mode bridge
sudo ip addr add 192.168.0.179/24 dev macvlan0
sudo ip link set macvlan0 up
sudo ip route add 192.168.0.178/32 dev macvlan0
