## Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
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
### Media conversion
###

# Origin <https://github.com/gotbletu/dotfiles/blob/master/aliasrc/.aliasrc>.

wi-convert-to-audio-mp3() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.m4a"
    echo -e "$0 file1.m4a file2.m4a file3.m4a"
    echo -e "$0 *.m4a"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".mp3 ]
    do
      ffmpeg -i "$arg" -codec:a libmp3lame -qscale:a 2 "${arg%.*}".mp3
    done
  done
}

wi-convert-to-audio-m4a() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp3"
    echo -e "$0 file1.mp3 file2.mp3 file3.mp3"
    echo -e "$0 *.mp3"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".m4a ]
    do
      ffmpeg -i "$arg" -codec:a aac -qscale:a 6 -strict experimental "${arg%.*}".m4a
    done
  done
}

wi-convert-to-audio-ogg() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp3"
    echo -e "$0 file1.mp3 file2.mp3 file3.mp3"
    echo -e "$0 *.mp3"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".ogg ]
    do
      ffmpeg -i "$arg" -codec:a libvorbis -qscale:a 5 -vn "${arg%.*}".ogg
    done
  done
}

wi-convert-to-audio-wav() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp3"
    echo -e "$0 file1.mp3 file2.mp3 file3.mp3"
    echo -e "$0 *.mp3"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".wav ]
    do
      ffmpeg -i "$arg" "${arg%.*}".wav
    done
  done
}

wi-convert-to-audio-webm() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp3"
    echo -e "$0 file1.mp3 file2.mp3 file3.mp3"
    echo -e "$0 *.mp3"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".webm ]
    do
      ffmpeg -i "$arg" -codec:a libvorbis -qscale:a 5 -vn "${arg%.*}".webm
    done
  done
}

wi-convert-to-audio-wavmono() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp3"
    echo -e "$0 file1.mp3 file2.mp3 file3.mp3"
    echo -e "$0 *.mp3"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}"-monoaudiotrack.wav ]
    do
      ffmpeg -i "$arg" -codec:a pcm_mulaw -ar 8000 -ac 1 "${arg%.*}"-monoaudiotrack.wav
    done
  done
}

wi-convert-to-audio-wma() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp3"
    echo -e "$0 file1.mp3 file2.mp3 file3.mp3"
    echo -e "$0 *.mp3"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".wma ]
    do
      ffmpeg -i "$arg" -codec:a wmav2 -b:a 128k "${arg%.*}".wma
    done
  done
}

wi-convert-to-video-x265() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.avi"
    echo -e "$0 file1.avi file2.avi file3.avi"
    echo -e "$0 *.avi"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*} x265.mkv" ]
    do
      # very clear (same as OG)
      # ffmpeg -i "$arg" -c:v libx265 -preset medium -crf 23 -x265-params lossless -c:a copy -c:s copy "${arg%.*} x265 crf23.mkv"
      # smaller file size, semi clear to OG
      ffmpeg -i "$arg" -c:v libx265 -preset medium -crf 28 -x265-params lossless -c:a copy -c:s copy "${arg%.*} x265.mkv"
    done
  done
}

wi-convert-to-video-mp4-h265() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.avi"
    echo -e "$0 file1.avi file2.avi file3.avi"
    echo -e "$0 *.avi"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".mp4 ]
    do
      ffmpeg -i "$arg" -codec:v libx265 -preset medium -crf 28 -codec:a aac -qscale:a 6 "${arg%.*}".mp4
    done
  done
}

wi-convert-to-video-mp4() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.avi"
    echo -e "$0 file1.avi file2.avi file3.avi"
    echo -e "$0 *.avi"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".mp4 ]
    do
      ffmpeg -i "$arg" -codec:v libx264 -preset medium -crf 22 -codec:a aac -qscale:a 6 -strict experimental "${arg%.*}".mp4
    done
  done
}

wi-convert-to-video-avi() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp4"
    echo -e "$0 file1.mp4 file2.mp4 file3.mp4"
    echo -e "$0 *.mp4"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".avi ]
    do
      ffmpeg -i "$arg" -codec:v mpeg4 -vtag xvid -qscale:v 3 -codec:a libmp3lame -qscale:a 4 "${arg%.*}".avi
    done
  done
}

wi-convert-to-video-ogv() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp4"
    echo -e "$0 file1.mp4 file2.mp4 file3.mp4"
    echo -e "$0 *.mp4"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".ogv ]
    do
      ffmpeg -i "$arg" -codec:v libtheora -qscale:v 7 -codec:a libvorbis -qscale:a 5 "${arg%.*}".ogv
    done
  done
}

wi-convert-to-video-mpg() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp4"
    echo -e "$0 file1.mp4 file2.mp4 file3.mp4"
    echo -e "$0 *.mp4"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".mpg ]
    do
      ffmpeg -i "$arg" -codec:v mpeg2video -qscale:v 2 -codec:a mp2 -b:a 192k "${arg%.*}".mpg
    done
  done
}

wi-convert-to-video-mkv() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp4"
    echo -e "$0 file1.mp4 file2.mp4 file3.mp4"
    echo -e "$0 *.mp4"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".mkv ]
    do
      ffmpeg -i "$arg" -codec:v libx264 -preset medium -crf 22 -codec:a libvorbis -qscale:a 5 "${arg%.*}".mkv
    done
  done
}

wi-convert-to-video-webm() {
  if [ $# -lt 1 ]; then
    echo -e "Usage: $0 <filename>"
    echo -e "\nExample:\n$0 file.mp4"
    echo -e "$0 file1.mp4 file2.mp4 file3.mp4"
    echo -e "$0 *.mp4"
    return 1
  fi
  myArray=( "$@" )
  for arg in "${myArray[@]}"; do
    while [ ! -f "${arg%.*}".webm ]
    do
      ffmpeg -i "$arg" -codec:v libvpx -crf 10 -b:v 1M -codec:a libvorbis "${arg%.*}".webm
    done
  done
}

### 
###
### Misc
###

export CHICKEN_REPOSITORY=~/.eggs/lib/chicken/8
export CHICKEN_DOC_REPOSITORY=/home/natsu/.eggs/share/chicken-doc

export EDITOR='emacsclient'
export BROWSER='conkeror'
export MANWIDTH=80

export INFOPATH="$HOME/src/guix/doc${INFOPATH:+:}$INFOPATH"

export GUILE_LOAD_PATH="${GUILE_LOAD_PATH}\
:$HOME/.guix-profile/share/guile/site/2.2/"

GUIX_LATEST=.config/guix/latest
ROOT_GUIX_LATEST=/root/$GUIX_LATEST

wi-guix-graph ()
{
    guix graph --type=references "$1" | dot -Tsvg > "$2.svg"
}

wi-guix-search-recsel ()
{
    guix package -s "$1" | recsel -p name,synopsis,homepage
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

wi-wget-guix-berlin ()
{
    wget "https://berlin.guixsd.org/nar/gzip/$1"
}

wi-wget-mirror-site ()
{
    wget --mirror -p --convert-links -P /tmp $1
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

wi-licensecheck ()
{
    "$(~/src/guix-wip-licensecheck/pre-inst-env guix build licensecheck)\
/bin/licensecheck" $@
}

wi-guix-build-natsu ()
{
    ./pre-inst-env \
        env \
        GUIX_PACKAGE_PATH= \
        GUILE_LOAD_PATH=$HOME/dotfiles/fiore:$GUILE_LOAD_PATH \
        guix build --no-grafts \
        --expression='(@ (guix-packages) guix-collection-packages)'
}

alias wi-list-bindings="bind -P"
alias wi-list-functions="compgen -A function"
alias wi-show-command="command -V"

export GUIX_PACKAGE_PATH=$HOME/src/guix-wigust

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
alias wi-guile-guix='GUILE_LOAD_PATH=$HOME/dotfiles/fiore/:$HOME/src/guix-wigust/:$GUILE_LOAD_PATH guile --no-auto-compile'
alias wi-guile-no-autocompile="guile --no-auto-compile"
alias wi-guix-configure="./configure --localstatedir=/var --prefix=''"
alias wi-guix-environment='guix environment --pure guix --ad-hoc help2man strace git gdb'
alias wi-guix-package-manifest='nohup guix package -m $HOME/dotfiles/fiore/packages.scm &> /tmp/guix-package-manifest-$(date "+%F").log &'
alias wi-guix-package-update='nohup guix package -u . &> /tmp/guix-package-update-$(date "+%F").log &'
alias wi-guix-reconfigure='sudo nohup guix system reconfigure -c 0 magnolia.scm &>/home/natsu/guix-reconfigure.log &'
alias wi-guix-refresh='./pre-inst-env env GUILE_LOAD_PATH=$HOME/dotfiles/fiore/:$GUILE_LOAD_PATH GUIX_PACKAGE_PATH= GUIX_GITHUB_TOKEN=$(cat .github-token) guix refresh -m ~/dotfiles/fiore/guix-manifest.scm'
alias wi-guix-wigust='GUIX_PACKAGE_PATH=$HOME/src/guix-wigust guix'
alias wi-history-grep="history | grep"
alias wi-mpv-quite='mpv --msg-level=all=no --no-resume-playback --keep-open=no'
alias wi-ps-tree="ps -ejH"
alias wi-remove-dublicate-lines="awk '!x[$0]++'"
alias wi-root-guix-pull='sudo guix pull --url=file:///home/natsu/src/guix --branch=wip-cgit -c 0'
alias wi-show-functions="declare -f"
alias wi-show-ip="curl http://ipecho.net/plain"
alias wi-trans-en="trans en:ru"
alias wi-trans-ru="trans ru:en"
alias wi-xclip-kill-clipboard="xclip -i -selection clipboard"
alias wi-youtube-dl-proxy='youtube-dl --proxy "socks5://localhost:9050/"'
