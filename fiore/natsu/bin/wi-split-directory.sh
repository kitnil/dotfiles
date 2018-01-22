#!/bin/sh

# Code from <https://stackoverflow.com/a/34463802>.

i=1; while read l; do mkdir $i; mv $l $((i++)); done< <(ls | xargs -n20)
