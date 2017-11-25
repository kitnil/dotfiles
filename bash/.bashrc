## Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
## Released under the GNU GPLv3 or any later version.

if [ -f "/etc/skel/.bashrc" ]
then
  # Load the skel profile's settings.
  . "/etc/skel/.bashrc"
fi

### 
###
### My staff
###

export INFOPATH="$HOME/src/guix/doc${INFOPATH:+:}$INFOPATH"

export GUILE_LOAD_PATH="${GUILE_LOAD_PATH}\
:$HOME/.guix-profile/share/guile/site/2.2/"

export EDITOR='emacsclient'
export MANWIDTH=80

wi-guix-graph ()
{
    guix graph --type=references "$1" \
        | dot -Gsize="10,10" -Gratio=0.7 -Tpng -Nfontsize=48 > "$2.png";
}

wi-guix-search-recsel ()
{ guix package -s "$1" | recsel -p name,synopsis,homepage; }

unalias ls
alias wi-disable-history="set +o history" # Only for current session.
alias wi-feh-svg="feh --magick-timeout 10"
alias wi-guile="guile --no-auto-compile"
alias wi-mpv-q='mpv --msg-level=all=no --no-resume-playback --keep-open=no'
alias wi-rdiff-backup-home='screen rdiff-backup $HOME "/srv/backup$HOME"'
alias wi-tse="trans en:ru"
alias wi-tsr="trans ru:en"
alias wi-xclip-clipboard="xclip -i -selection clipboard"
alias wi-youtube-dl-proxy='youtube-dl --proxy "socks5://localhost:9050/"'
alias wi-guix-environment-guix='guix environment --pure guix --ad-hoc help2man'
alias wi-guix-configure="./configure --localstatedir=/var --prefix=''"
alias emacs-no-x="emacs -nw"
