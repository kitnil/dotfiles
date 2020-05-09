#!/bin/bash
# 1. Create autossh user.
# 2. Generate SSH config and autossh systemd unit.

set -e -x

REMOTE_PORT="$1"
REMOTE_USER="$2"

for program in autossh cat useradd sudo systemctl; do
    command -v "$program" || exit
done

useradd -m autossh

if [ ! -f "/home/autossh/.ssh/id_rsa" ]
then
    sudo -u autossh -i ssh-keygen -b 4096 -m pem
fi

if [ ! -f "/home/autossh/.ssh/config" ]
then
    cat > "/home/autossh/.ssh/config" << EOF
Host guix.duckdns.org
User $REMOTE_USER
RemoteForward $REMOTE_PORT localhost:22

Compression yes
StrictHostKeyChecking no
UserKnownHostsFile /dev/null
#ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3
EOF
fi
chown autossh: /home/autossh/.ssh/config

if [ ! -f "/etc/systemd/system/autossh@.service" ]
then
cat > "/etc/systemd/system/autossh@.service" << EOF
[Unit]
Description=Keeps an ssh tunnel to %I open
After=network-online.target ssh.service

[Service]
User=autossh
# no monitoring
Environment="AUTOSSH_PORT=0"
# Disable gatetime behaviour
Environment="AUTOSSH_GATETIME=0"
EnvironmentFile=/etc/default/autossh@%i
RestartSec=3
Restart=always

# -NT Just open the connection and do nothing (not interactive, no tty alloc)
# use /usr/bin/ssh instead of autossh is good as well
ExecStart=/usr/bin/autossh -NT -o "ExitOnForwardFailure=yes" \$SSH_OPTIONS \${TARGET_HOST}
TimeoutStopSec=10

[Install]
WantedBy=multi-user.target
EOF
fi

if [ ! -f "/etc/default/autossh@guix" ]
then
cat > "/etc/default/autossh@guix" << EOF
# Options for autossh@host1.service
# Place it at /etc/default

# Save all your credential/user/port related config in ~/.ssh/config is strongly recommanded
# Leave hostname here only
TARGET_HOST=guix.duckdns.org

# === Settings below for ADVANCED users only ===

SSH_OPTIONS=-o "ServerAliveInterval=10" -o "ServerAliveCountMax=3"
AUTOSSH_PORT=0
AUTOSSH_GATETIME=0
EOF
fi
