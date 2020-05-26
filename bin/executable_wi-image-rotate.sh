#!/bin/sh

for file in *; do convert $file -rotate 180 $file; done
