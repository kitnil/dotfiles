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

function stat-link { stat --format=%N $@; }

# Origin <https://www.fsf.org/blogs/directory/the-free-software-directory-needs-you-irc-meetups-every-friday-1>.
#
# Every Friday at 12:00-15:00 EDT (16:00 to 19:00 UTC)
# meet on IRC in the #fsf channel on irc.freenode.org
date-fsf() { date --date='TZ="America/New_York" 12:00 this Fri'; }

cl() { echo $(tput cols)x$(tput lines); }

gpa() { guix package -A $@ | awk '{ print $1"-"$2 }'; }
