#!/bin/sh

# Prepare an environment to build Guix

rm build-env

env GUIX_PACKAGE_PATH= guix environment --root=build-env --pure guix --ad-hoc help2man guile-sqlite3 guile-gcrypt "$@"
# strace git gdb
