#!/bin/sh

# Get mailing list

target=/tmp/$0

rsync -rltpHS --delete-excluded rsync://lists.gnu.org/mbox/$0 $target

output=/tmp/lists

for file in ~/MailingListArchives/*; do
    mkdir -p $output/$dir/$(basename $file)
    mb2md -s $file -d $output/$(basename $file)
done
