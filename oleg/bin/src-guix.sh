#!/bin/sh

set -o errexit
set -o nounset
set -o xtrace
set -o pipefail

./bootstrap
./configure --localstatedir=/var --prefix=
make
make check
make check-system TESTS="cgit dovecot rsnync zabbix"
