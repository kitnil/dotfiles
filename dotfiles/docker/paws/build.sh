#!/usr/bin/env bash

set -o errexit
set -o xtrace
set -o pipefail

INPUTS=(
    build-essential
    cpanminus
    git
    libdatetime-perl
    libfile-slurp-perl
    libhtml-parser-perl
    libhttp-async-perl
    libio-async-perl
    libio-async-ssl-perl
    libio-capture-perl
    libipc-run3-perl
    libipc-shareable-perl
    libjson-xs-perl
    libmime-tools-perl
    libwww-perl
    libyaml-perl
    perl
    sendmail
)

apt-get update
apt-get install --yes "${INPUTS[@]}"

git clone https://github.com/tomhrr/paws /usr/local/src/paws
cd /usr/local/src/paws
perl Makefile.PL
make
make install

cpanm Net::HTTPS::NB
cpanm Net::Async::WebSocket::Client constant::override

groupadd --gid 998 user
useradd --uid 1000 --gid 998 --create-home user
mkdir /home/user/.paws
chown user: /home/user/.paws
