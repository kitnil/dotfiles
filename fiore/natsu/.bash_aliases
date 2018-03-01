## Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
## Released under the GNU GPLv3 or any later version.

if [ -f "/etc/skel/.bashrc" ]
then
  # Load the skel profile's settings.
  . "/etc/skel/.bashrc"
fi

wi-guix-graph ()
{
    guix graph --type=references "$1" | dot -Tsvg > "$2.svg"
}

wi-guix-search-recsel ()
{
    guix package -s "$1" | recsel -p name,synopsis,homepage
}

wi-git-hash ()
{
    git -C $1 rev-parse HEAD
}

alias wi-pre-guix-guile='./pre-inst-env\
 env GUIX_PACKAGE_PATH= guile --no-auto-compile'

alias wi-guile-guix='GUILE_LOAD_PATH=$HOME/dotfiles/fiore/\
:$HOME/src/guix-wigust/:$GUILE_LOAD_PATH guile --no-auto-compile'

alias wi-guile-no-autocompile="guile --no-auto-compile"

alias wi-guix-configure="./configure --localstatedir=/var --prefix=''"

alias wi-guix-environment='guix environment --pure guix\
 --ad-hoc help2man strace git gdb'

alias wi-guix-package-manifest='nohup guix package\
 -m $HOME/dotfiles/fiore/packages.scm\
 &> /tmp/guix-package-manifest-$(date "+%F").log &'

alias wi-guix-package-update='nohup guix package\
 -u . &> /tmp/guix-package-update-$(date "+%F").log &'

alias wi-guix-reconfigure='sudo guix system reconfigure -c 0\
 $HOME/dotfiles/fiore/magnolia.scm'

alias wi-guix-refresh='./pre-inst-env env\
 GUILE_LOAD_PATH=$HOME/dotfiles/fiore/:$GUILE_LOAD_PATH\
 GUIX_PACKAGE_PATH=\
 GUIX_GITHUB_TOKEN=$(cat .github-token)\
 guix refresh -m ~/dotfiles/fiore/guix-manifest.scm'

alias wi-guix-system-build='./pre-inst-env\
 env GUIX_PACKAGE_PATH=$HOME/src/guix-wigust\
 guix system build ~/dotfiles/fiore/magnolia.scm'

alias wi-pre-guix-reconfigure='sudo -E ./pre-inst-env\
 env GUIX_PACKAGE_PATH=$HOME/src/guix-wigust\
 guix system reconfigure $HOME/dotfiles/fiore/magnolia.scm'

alias wi-guix-hash='guix hash -rx $1'
alias wi-wget-guix-berlin='wget "https://berlin.guixsd.org/nar/gzip/$1"'

alias wi-sudo-guix-pull='sudo guix pull\
 --url=file:///home/natsu/src/guix --branch=wip-cgit -c 0'

alias wi-backup-home='duplicity --no-encryption\
 --exclude $HOME/.cache $HOME file:///srv/backup'

alias wi-wget-mirror-site='wget --mirror -p --convert-links -P /tmp $1'

alias wi-pipe-emacs='emacs -nw --insert <(cat) </dev/tty'
alias wi-etags-el="find . -name '*.el' -print | etags -"
alias wi-etags-c="find . -name '*.[ch]' -print | etags --append -"

alias wi-youtube-dl-uploader="youtube-dl\
 --output '/srv/videos/%(uploader_id)s/%(title)s-%(id)s.%(ext)s' $URL"

alias wi-youtube-dl-proxy='youtube-dl --proxy "socks5://localhost:9050/"'

alias wi-licensecheck='$(~/src/guix-wip-licensecheck/pre-inst-env \
guix build licensecheck)/bin/licensecheck $@'

alias wi-guix-build-natsu="./pre-inst-env env\
 GUIX_PACKAGE_PATH=\
 GUILE_LOAD_PATH=$HOME/dotfiles/fiore:$GUILE_LOAD_PATH\
 guix build --no-grafts\
 --expression='(@ (guix-packages) guix-collection-packages)'"

alias guix-pre-build-lint-install-package="./pre-inst-env env\
 GUIX_PACKAGE_PATH=\
 guix build --no-grafts $PACKAGE --substitute-urls='https://berlin.guixsd.org'\
 && ./pre-inst-env env\
 GUIX_PACKAGE_PATH= guix lint $PACKAGE\
 && ./pre-inst-env env\
 GUIX_PACKAGE_PATH= guix package -i $PACKAGE\
 --substitute-urls='https://berlin.guixsd.org'\
 -p guix-wip-$PACKAGE-profile"

alias wi-guix-bootstrap-configure-make="./bootstrap\
 && ./configure --localstatedir=/var --prefix=\
 && make -j 5"

alias wi-list-bindings="bind -P"
alias wi-list-functions="compgen -A function"
alias wi-show-command="command -V"
alias wi-show-functions="declare -f"

unalias ls
unalias grep

alias wi-df-total="df -Tha --total"
alias wi-disable-history="set +o history" # Only for current session.
alias wi-emacs-no-x="emacs -nw"
alias wi-emacs-org-video="emacs -nw ~/org/video.org"
alias wi-feh-svg="feh --magick-timeout 10"
alias wi-free-human="free -ht"
alias wi-git-show-contributers='PAGER= git shortlog -sne'
alias wi-git-worktree-list='git worktree list --porcelain'

alias wi-history-grep="history | grep"
alias wi-mpv-quite='mpv --msg-level=all=no --no-resume-playback --keep-open=no'
alias wi-ps-tree="ps -ejH"
alias wi-pgrep-full='pgrep -fa'
alias wi-remove-dublicate-lines="awk '!x[$0]++'"

alias wi-show-ip="wget -qO- http://ipecho.net/plain"
alias wi-cheat='wget -qO- cheat.sh/'

alias wi-trans-en="trans en:ru"
alias wi-trans-ru="trans ru:en"
alias wi-trans-en-shell='trans -shell en:ru'
alias wi-trans-ru-shell='trans -shell ru:en'

alias wi-xclip-kill-clipboard="xclip -i -selection clipboard"
alias wi-screen="exec screen"

# Origin <https://directory.fsf.org/wiki/Template:IRC_text>
# date --date='TZ="America/Detroit" 12:00 this Fri'

alias wi-cool-retro-term='$(guix build cool-retro-term)/bin/cool-retro-term'
alias wi-st='$(guix build st)/bin/st'

alias wi-iptables-show="sudo iptables -n -L"

# Origin <https://stackoverflow.com/a/34463802>.
alias wi-split-directory='i=1; while read l; do mkdir $i; mv $l $((i++));\
 done< <(ls | xargs -n40)'

alias wi-wget-mirror="wget -m --random-wait -e robots=off\
 -U mozilla --follow-tags=a -k -a wget.log --html-extension -A.html,.php\
 --base="

# Origin <https://github.com/gotbletu/dotfiles/blob/master/aliasrc/.aliasrc#L1152>.
wget-extension()
{
    wget -r -l1 -H -t1 -nd -N -np -A "$1" -erobots=off "$2"
}

alias wi-grep-urls="grep -rh -Po '(?<=href=\")[^\"]*' ."

alias guix-narinfo-cache-directory="guile -c '(display\
 (string-append (@@ (guix scripts substitute) %narinfo-cache-directory)\
 \"\\n\"))'"
