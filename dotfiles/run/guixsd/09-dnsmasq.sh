#!/usr/bin/env bash

exec sudo -i /gnu/store/l2dw3lfb2qxjp6bgrn431fxa428rgs1f-dnsmasq-2.90/sbin/dnsmasq --no-daemon --local-service --interface=enp34s0 --ipset=/play.google.com/tor --ipset=/youtube.com/googlevideo.com/byedpi --server=8.8.8.8 --no-resolv --bind-interfaces --except-interface=lo --except-interface=br154.br154 --except-interface=br0
