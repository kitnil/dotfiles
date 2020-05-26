#!/bin/sh

# Get mailing list

origin=/tmp/$1-origin

rsync -rltpHS --delete-excluded rsync://lists.gnu.org/mbox/$1/ $origin

for file in $origin/*; do
    [ -e $file ] || continue
    basefile=$(basename $file)
    mkdir -p $basefile
    mb2md -s $file -d $basefile
done
