#!/bin/sh

cat <<'EOF'
rsync --info=progress2 --sparse -a -H -A -X --numeric-ids 192.168.0.144:News .
rsync --info=progress2 --sparse -a -H -A -X --numeric-ids 192.168.0.144:.newsrc .
rsync --info=progress2 --sparse -a -H -A -X --numeric-ids 192.168.0.144:.newsrc.eld .
EOF
