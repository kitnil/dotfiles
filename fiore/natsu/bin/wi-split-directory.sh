#!/bin/sh

i=1; while read l; do mkdir $i; mv $l $((i++)); done< <(ls|xargs -n10)
