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

complete -W "$(echo `cat ~/.ssh/known_hosts | cut -f 1 -d ' ' | sed -e s/,.*//g | uniq | grep -v "\["`;)" ssh

function vm { ~/src/majordomo/cvm.py $1 | cut -d ' ' -f 2; }
