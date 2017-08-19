#!/bin/sh
exec mbsync -a && notmuch new --quiet
