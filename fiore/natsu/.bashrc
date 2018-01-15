## Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
## Released under the GNU GPLv3 or any later version.

if [ -f "/etc/skel/.bashrc" ]
then
  # Load the skel profile's settings.
  . "/etc/skel/.bashrc"
fi

### 
###
### Nix
###

export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"
export MANPATH=$MANPATH:$HOME/.nix-profile/share/man

### 
###
### Misc
###

export EDITOR='emacsclient'
export MANWIDTH=80

export INFOPATH="$HOME/src/guix/doc${INFOPATH:+:}$INFOPATH"

export GUILE_LOAD_PATH="${GUILE_LOAD_PATH}\
:$HOME/.guix-profile/share/guile/site/2.2/"

GUIX_LATEST=.config/guix/latest
ROOT_GUIX_LATEST=/root/$GUIX_LATEST

guix-graph ()
{
    guix graph --type=references "$1" \
        | dot -Gsize="10,10" -Gratio=0.7 -Tpng -Nfontsize=48 > "$2.png"
}

guix-search-recsel ()
{
    guix package -s "$1" \
     | recsel -p name,synopsis,homepage
}

backup-home ()
{
    duplicity --no-encryption --exclude $HOME/.cache $HOME file:///srv/backup
}

pipe-emacs ()
{
    emacs -nw -Q --insert <(cat) </dev/tty
}

guix-root-update ()
{
    unlink $ROOT_GUIX_LATEST
    ln -s $(readlink $HOME/$GUIX_LATEST) $ROOT_GUIX_LATEST
}

guix-system-build ()
{
    GUIX_PACKAGE_PATH=$HOME/src/guix-wigust ./pre-inst-env \
                     guix system build ~/dotfiles/fiore/magnolia.scm
}

pre-guix-reconfigure ()
{
    sudo -E ./pre-inst-env \
         env GUIX_PACKAGE_PATH=$HOME/src/guix-wigust \
         guix system reconfigure $HOME/dotfiles/fiore/magnolia.scm
}

guix-hash ()
{
    guix hash -rx $1
}

git-hash ()
{
    git -C $1 rev-parse HEAD
}

history-usage ()
{
    history \
        | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' \
        | grep -v "./" \
        | column -c3 -s " " -t \
        | sort -nr \
        | nl
}

etags-el ()
{
    find . -name '*.el' -print | etags -
}

etags-c ()
{
    find . -name '*.[ch]' -print | etags --append -
}

youtube-dl-uploader ()
{
    URL=$1
    youtube-dl --output '/srv/videos/%(uploader_id)s/%(title)s-%(id)s.%(ext)s' $URL
}

export GUIX_PACKAGE_PATH=$HOME/src/guix-wigust

unalias ls
unalias grep

alias df-total="df -Tha --total"
alias disable-history="set +o history" # Only for current session.
alias emacs-no-x="emacs -nw"
alias emacs-org-video="emacs -nw ~/org/video.org"
alias feh-svg="feh --magick-timeout 10"
alias free-human="free -ht"
alias git-show-contributers='PAGER= git shortlog -sne'
alias guile-no-autocompile="guile --no-auto-compile"
alias guix-configure="./configure --localstatedir=/var --prefix=''"
alias guix-environment='guix environment --pure guix --ad-hoc help2man strace git gdb'
alias guix-wigust='GUIX_PACKAGE_PATH=$HOME/src/guix-wigust guix'
alias history-grep="history | grep"
alias list-bindings="bind -P"
alias list-functions="compgen -A function"
alias mpv-quite='mpv --msg-level=all=no --no-resume-playback --keep-open=no'
alias ps-tree="ps -ejH"
alias remove-dublicate-lines="awk '!x[$0]++'"
alias root-guix-pull='sudo guix pull --url=file:///home/natsu/src/guix --branch=wip-cgit -c 0'
alias show-functions="declare -f"
alias show-ip="curl http://ipecho.net/plain"
alias trans-en="trans en:ru"
alias trans-ru="trans ru:en"
alias xclip-kill-clipboard="xclip -i -selection clipboard"
alias youtube-dl-proxy='youtube-dl --proxy "socks5://localhost:9050/"'
export CHICKEN_REPOSITORY=~/.eggs/lib/chicken/8
export CHICKEN_DOC_REPOSITORY=/home/natsu/.eggs/share/chicken-doc
alias git-worktree-list='git worktree list --porcelain'
