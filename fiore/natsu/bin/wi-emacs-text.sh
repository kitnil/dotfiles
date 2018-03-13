#!/bin/sh
# Edit text with Emacs and paste it to GUI's clipboard.

emacsclient -c -e '(anywhere-create)' "$@"
