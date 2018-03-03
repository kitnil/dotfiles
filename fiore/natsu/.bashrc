## Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
## Released under the GNU GPLv3 or any later version.

if [ -f "/etc/skel/.bashrc" ]
then
    # Load the skel profile's settings.
    . "/etc/skel/.bashrc"
fi

if [ -f "$HOME/.bash_aliases" ]
then
    # Load the Bash aliases and functions.
    . "$HOME/.bash_aliases"
fi

