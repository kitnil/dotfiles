# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

export LC_TIME=en_GB.UTF-8

# Added by Nix installer.
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh;
fi

export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"
export MANPATH=$MANPATH:$HOME/.nix-profile/share/man

export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale

export CHICKEN_REPOSITORY=~/.eggs/lib/chicken/8
export CHICKEN_DOC_REPOSITORY=/home/natsu/.eggs/share/chicken-doc

export EDITOR='emacsclient'
export BROWSER='firefox'
export MANWIDTH=80

export INFOPATH="$HOME/src/guix/doc${INFOPATH:+:}$INFOPATH"

export PATH=$HOME/bin:$HOME/.npm-global/bin:$PATH

export GUILE_WARN_DEPRECATED=no

# TODO: readlink could fail to enter `guix environment`
# alias wi-pure-bash="env -i $(readlink $(which bash)) --noprofile --norc"

# TODO: Escape window names with asterisks.
# wi-x-resize-window ()
# {
#     window_name=$(xwininfo | grep 'Window id' | awk '{ print $5 }')
#     xdotool search --name "$window_name" windowsize $1 $2
# }

# Fix mouse wheel in gtk3
# https://github.com/stumpwm/stumpwm/wiki/FAQ
export GDK_CORE_DEVICE_EVENTS=1

export RESTIC_REPOSITORY=/srv/backup/magnolia
