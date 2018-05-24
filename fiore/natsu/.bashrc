## Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
## Released under the GNU GPLv3 or any later version.

if [ -f "/etc/skel/.bashrc" ]
then
    # Load the skel profile's settings.
    . "/etc/skel/.bashrc"
fi

if [ -f ".bash_aliases" ]
then
    # Load the Bash aliases and functions.
    . ".bash_aliases"
fi

function man-in-emacs { emacsclient --eval "(man \"$1\")"; }
function man-to-pdf { man -t "$1" | ps2pdf - "$1.pdf"; }
