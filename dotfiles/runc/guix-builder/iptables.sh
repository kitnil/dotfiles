iptables -A INPUT -s 192.168.10.0/24 -j ACCEPT

iptables -t nat -A POSTROUTING -o runc0 -j MASQUERADE
iptables -A FORWARD -i runc0 -o enp34s0 -m state --state RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i enp34s0 -o runc0 -j ACCEPT
iptables -A FORWARD -i runc0 -o enp34s0 -j ACCEPT

# access to majordomo vpn (for gitlab)
iptables -A FORWARD -i tapvpn -o runc0 -j ACCEPT
iptables -A FORWARD -i runc0 -o tapvpn -j ACCEPT
