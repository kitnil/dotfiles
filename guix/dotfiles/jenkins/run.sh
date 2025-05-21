#!/bin/sh
set -e -x
unset PYTHONPATH
jenkins-jobs update jobs.yml
