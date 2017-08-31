#!/bin/sh

# Get mailing list

origin=/tmp/$1-origin
output=/tmp/$1

rsync -rltpHS --delete-excluded rsync://lists.gnu.org/mbox/$1/ /tmp/$1-origin

for f in $origin/*; do
    [ -e $f ] || continue
    mkdir -p $output/$(basename $f)
    mb2md -s $f -d $output/$(basename $f)
done
