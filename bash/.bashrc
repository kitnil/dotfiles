# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [ -n "$SSH_CLIENT" -a -z "`type -P cat`" ]
then
    # We are being invoked from a non-interactive SSH session
    # (as in "ssh host command") but 'cat' cannot be found
    # in $PATH.  Source /etc/profile so we get $PATH and other
    # essential variables.
    source /etc/profile
fi

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n "$GUIX_ENVIRONMENT" ]
then
    PS1='\u@\h \w [env]\$ '
else
    PS1='\u@\h \w\$ '
fi

alias ls='ls -p --color'
alias ll='ls -lh'
alias la='ls -a'
alias l1='ls -1'

alias dir='ls -ba'

alias ss="ps -aux"
alias dot='ls .[a-zA-Z0-9_]*'

alias c="clear"
alias m="more"
alias j="jobs"

alias suspend='sudo loginctl suspend'

export EDITOR=emacsclient

export GPG_TTY=$(tty)

export GUILE_LOAD_COMPILED_PATH="${GUILE_LOAD_COMPILED_PATH}\
:/run/current-system/profile/lib/guile/2.2/site-ccache\
:/run/current-system/profile/share/guile/site/2.2"

export GUILE_LOAD_PATH="${GUILE_LOAD_PATH}\
:/run/current-system/profile/share/guile/site/2.2"

export GUIX_PACKAGE_PATH="/home/natsu/src/guix-wigust"
export GUIX_PACKAGE_PATH="${GUIX_PACKAGE_PATH}:/home/natsu/src/ng0-packages"

alias guix-upgrade="guix package -m $HOME/dotfiles/guix/user.scm -u"

alias guix-configure="./configure --localstatedir=/var --prefix=''"
alias guix-make='guix environment guix --ad-hoc help2man -- make'
alias guix-make-tags='guix environment guix -- make tags'

alias guix-system-reconfigure="sudo GUILE_LOAD_PATH=\"$HOME/src/iptables\
:$GUILE_LOAD_PATH\" guix system reconfigure \
$HOME/dotfiles/guix/system-magnolia.scm"

alias guix-system-build="GUILE_LOAD_PATH=\"$HOME/src/iptables\
:$GUILE_LOAD_PATH\" guix system build \
$HOME/dotfiles/guix/system-magnolia.scm"

alias guix-pull="guix pull --url=git://magnolia.local/~natsu/src/guix"
alias pre-guix="$HOME/src/guix/pre-inst-env guix"
alias feh-svg="feh --magick-timeout 10"
