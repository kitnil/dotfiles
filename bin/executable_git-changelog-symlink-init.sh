#!/usr/bin/env bash

# Initialize current Git directory for vc-dwim

d=.git/c
test -d .git || return 1
mkdir $d
touch $d/ChangeLog
(cd $d && git init && git add ChangeLog && git commit -m. -a)
ln --backup -s $d/ChangeLog .
