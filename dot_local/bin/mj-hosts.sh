#!/bin/sh

set -e -x

# ssh router <<< busybox 'grep 172.16. /tmp/etc/hosts.dnsmasq' && exit 0
dig @172.16.103.2 -tAXFR intr |  awk '/\tA\t/ {print $NF" "$1}' |         sort   | sed -e 's|.intr.||g' | awk '{print $1" "$2" "$2".intr"}' > /tmp/mj.zone
dig @172.16.103.2 -tAXFR  intr | awk '/CNAME/ {print system("dig +short "$NF "| tail -1 | xargs echo -n ")" " $1}' | sed -e 's:0 : :' -e 's|.intr.||' | awk '/172/ {print $1" "$2" "$2".intr"}' >> /tmp/mj.zone
# scp /tmp/mj.zone router:/tmp/
# ssh router <<< busybox 'grep 172.16. /tmp/etc/hosts.dnsmasq' || ssh router <<< busybox 'cat /tmp/mj.zone >> /tmp/etc/hosts.dnsmasq'
# ssh router <<< busybox 'kill -HUP $(pidof dnsmasq)'
# ssh router <<< busybox 'rm /tmp/mj.zone'
#ssh router <<< busybox 'touch /tmp/mjlocked'
# rm -f /tmp/mj.zone
