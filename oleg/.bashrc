## Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

function stat-link { stat --format=%N $@; }

# Origin <https://www.fsf.org/blogs/directory/the-free-software-directory-needs-you-irc-meetups-every-friday-1>.
#
# Every Friday at 12:00-15:00 EDT (16:00 to 19:00 UTC)
# meet on IRC in the #fsf channel on irc.freenode.org
date-fsf() { date --date='TZ="America/New_York" 12:00 this Fri'; }

cl() { echo $(tput cols)x$(tput lines); }

gpa() { guix package -A $@ | awk '{ print $1"-"$2 }'; }

if [ -d "/run/current-system" ]
then
    true
else
    . "$HOME/.guix-profile/etc/profile"

    if [[ -f "$GUIX_PROFILE/etc/profile" ]]; then
        . "$GUIX_PROFILE/etc/profile"
    fi

    export BROWSER='firefox'
    export GUILE_WARN_DEPRECATED=no
    export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
    export GUIX_PROFILE="$HOME/.guix-profile"
    export PATH="$HOME/.config/guix/current/bin${PATH:+:}$PATH"
    export PATH=$HOME/bin:$HOME/.npm-global/bin:$PATH
    export INFOPATH="$HOME/src/guix/doc${INFOPATH:+:}$INFOPATH"

    # Fix mouse wheel in gtk3
    # https://github.com/stumpwm/stumpwm/wiki/FAQ
    export GDK_CORE_DEVICE_EVENTS=1

    XDG_CONFIG_DIRS=/etc/xdg/xdg-cinnamon:/etc/xdg
    #XDG_CURRENT_DESKTOP=X-Cinnamon
    XDG_DATA_DIRS=/usr/share/cinnamon:/usr/share/gnome:/usr/local/share/:/usr/share/
    #XDG_GREETER_DATA_DIR=/var/lib/lightdm-data/oleg
    #XDG_RUNTIME_DIR=/run/user/1001
    #XDG_SEAT=seat0
    #XDG_SEAT_PATH=/org/freedesktop/DisplayManager/Seat0
    #XDG_SESSION_DESKTOP=cinnamon
    #XDG_SESSION_ID=c18
    #XDG_SESSION_PATH=/org/freedesktop/DisplayManager/Session8
    #XDG_SESSION_TYPE=x11
    #XDG_VTNR=7

    export SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs"
    export SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
    export GIT_SSL_CAINFO="$SSL_CERT_FILE"
    export CURL_CA_BUNDLE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt"
fi

export LOCALE_ARCHIVE="$(readlink ~/.nix-profile/lib/locale)/locale-archive"
export MANPATH=$MANPATH:$HOME/.nix-profile/share/man

emc()
{
    emacsclient -n "$@"
    stumpish emacsclient
}
export EDITOR='emc'

export MANWIDTH=80

# TODO: readlink could fail to enter `guix environment`
# alias wi-pure-bash="env -i $(readlink $(which bash)) --noprofile --norc"

# TODO: Escape window names with asterisks.
# wi-x-resize-window ()
# {
#     window_name=$(xwininfo | grep 'Window id' | awk '{ print $5 }')
#     xdotool search --name "$window_name" windowsize $1 $2
# }

# TODO: gpg
export SLACK_CLI_TOKEN='***REMOVED***'
export JORD_PASS='***REMOVED***'
export CVM_USER='cron'
export CVM_PASS='***REMOVED***'
export GMS_USER='pyhalov'
export GMS_PASS='***REMOVED***'

ansible-host()
{
    ansible --inventory $1, $1 --become --ask-become-pass ${@:2}
}

ssh-sudo()
{
    ssh -t $2 -- "sudo --stdin --validate --prompt='' <<< $JORD_PASS \
&& sudo --user=$1 --login";
}

jord-vm-ip()
{
    gms vm ip $1 | recsel -pip_address | awk '{ print $2 }'
}

jord-ansible-service-start()
{
    vm="$(gms vm ip $1 | recsel -pip_address | awk '{ print $2 }')"
    ansible --user sup --private-key=$HOME/.ssh/id_rsa_sup --inventory $vm, \
            $vm --module-name service --args "name=$2 state=started" \
            --become --ask-become-pass

}

jord-web-loadavg()
{
    watch --color "seq -f 'web%gs' 15 37 | xargs guix environment -l $HOME/src/guile-loadavg/guix.scm -- $HOME/src/guile-loadavg/pre-inst-env loadavg weather"
}

jord-web-account-check()
{
    parallel --will-cite -k 'printf "domain: %s\n" {1}; curl --max-time 10 -L -s -o /dev/null -w "ip-address: %{remote_ip}\nstatus_code: %{http_code}" {1}; printf "\n\n"' ::: $(gms account website $1 | recsel -pname | awk '{ print $2 }')
}


guix-export-archive()
{
    path="$1"
    destination="$2"
    guix archive --export -r $path | ssh $destination guix archive --import
}

restic-grep-bash-history()
{
    user="$1"
    host="$2"
    pattern="$3"
    grep --no-filename $pattern /tmp/backup-$host/snapshots/*/home/$user/.bash_history | sort -u
}

ssh-keygen-rsa()
{
    file="$1"
    ssh-keygen -b 4096 -m pem -f "$HOME/.ssh/id_rsa_$file"
}

gpu()
{
    guix package --substitute-urls='http://cuirass.tld https://ci.guix.info https://mirror.hydra.gnu.org' --upgrade=. $@
}

activity()
{
    ps -ef | awk '{ print $1 }' | sort | uniq | wc -l;
}

# https://www.gnu.org/software/emacs/manual/html_node/efaq/Disabling-backups.html
alias ls='ls -B -p --color=auto'

if [ -f "$HOME/.guix-profile/etc/profile.d/nix.sh" ]
then
    . "$HOME/.guix-profile/etc/profile.d/nix.sh"
fi

if [ -f "$HOME/.nix-profile/share/bash-completion/completions/docker" ]
then
    . "$HOME/.nix-profile/share/bash-completion/completions/docker"
fi

if [ -f "$HOME/.bash_tmp" ]
then
    . "$HOME/.bash_tmp"
fi

alias bridge='bridge -color=always'
alias ip='ip -color=always'

jenkins-log()
{
    job="$1"
    curl --silent --user "admin:$(pass show magnolia/jenkins/admin)" \
         "https://jenkins.wugi.info/job/$job/lastBuild/consoleText"
}

jenkins-jobs()
{
    curl --silent --user "admin:$(pass show magnolia/jenkins/admin)" \
         https://jenkins.wugi.info/api/json/ \
        | jq -r '.jobs[] | [.name, .color] | @tsv'
}

jenkins-active-jobs()
{
    jenkins-jobs | grep --color=no red_anime
}

alias gpi='guix package -i'

guix-download-my-manifest()
{
    guix environment --substitute-urls='https://ci.guix.info http://cuirass.tld' \
         --manifest="$HOME/src/dotfiles/fiore/manifests/natsu-manifest.scm"
}

guix-apply-my-manifest()
{
    guix package --substitute-urls='https://ci.guix.info http://cuirass.tld' \
         --manifest="$HOME/src/dotfiles/fiore/manifests/natsu-manifest.scm"
}

grub-list-entries()
{
    awk -F\" '$1=="menuentry " {print i++ " : " $2}' /boot/grub/grub.cfg
}

guix-package-show()
{
    guix package --search="$1" | recsel -e "name=\"$1\""
}
