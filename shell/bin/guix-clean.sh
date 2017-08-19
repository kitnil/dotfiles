#!/bin/sh

exec find /var/guix/profiles/* -maxdepth 0 -mtime +14 -not -name per-user -exec sudo unlink {} \;
exec guix package --delete-generations=2w
