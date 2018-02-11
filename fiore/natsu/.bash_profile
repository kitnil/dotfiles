# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
# if [ -e /home/natsu/.nix-profile/etc/profile.d/nix.sh ]; then . /home/natsu/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export LC_TIME=en_GB.UTF-8

# Nix
export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"
export MANPATH=$MANPATH:$HOME/.nix-profile/share/man


export CHICKEN_REPOSITORY=~/.eggs/lib/chicken/8
export CHICKEN_DOC_REPOSITORY=/home/natsu/.eggs/share/chicken-doc

export EDITOR='emacsclient'
export BROWSER='conkeror'
export MANWIDTH=80

export INFOPATH="$HOME/src/guix/doc${INFOPATH:+:}$INFOPATH"

export GUILE_LOAD_PATH="${GUILE_LOAD_PATH}\
:$HOME/.guix-profile/share/guile/site/2.2/"

export GUIX_PACKAGE_PATH=$HOME/src/guix-wigust

# TODO: readlink could fail to enter `guix environment`
# alias wi-pure-bash="env -i $(readlink $(which bash)) --noprofile --norc"

# TODO: Escape window names with asterisks.
# wi-x-resize-window ()
# {
#     window_name=$(xwininfo | grep 'Window id' | awk '{ print $5 }')
#     xdotool search --name "$window_name" windowsize $1 $2
# }
