## Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

if [ -f "/run/current-system/profile/share/bash-completion/completions/ssh" ]
then
    . "/run/current-system/profile/share/bash-completion/completions/ssh"
    shopt -u hostcomplete && complete -F _ssh ssh slogin autossh sidedoor c
fi

export GRADLE_HOME=/opt/gradle

# if [ -f "$HOME/.guix-profile/etc/profile.d/autojump.sh" ]
# then
#     . "$HOME/.guix-profile/etc/profile.d/autojump.sh"
# fi

# # http://puzan.info/linux/2014-05-14-direnv.html
# if [ -f "$HOME/.guix-profile/bin/direnv" ]
# then
#     eval "$(direnv hook bash)"
# fi

man-in-emacs()
{
    emacsclient --eval "(man \"$1\")"
}

man-to-pdf()
{
    man -t "$1" | ps2pdf - "$1.pdf"
}

stat-link()
{
    stat --format=%N $@
}

# Origin <https://www.fsf.org/blogs/directory/the-free-software-directory-needs-you-irc-meetups-every-friday-1>.
#
# Every Friday at 12:00-15:00 EDT (16:00 to 19:00 UTC)
# meet on IRC in the #fsf channel on irc.freenode.org
date-fsf()
{
    date --date='TZ="America/New_York" 12:00 this Fri'
}

cl()
{
    echo $(tput cols)x$(tput lines)
}

gpa()
{
    guix package -A $@ | awk '{ print $1"-"$2 }'
}

export GUIX_GITHUB_TOKEN="***REMOVED***"
export GUIX_BUILD_OPTIONS="--no-grafts"

if [ -d "/run/current-system" ]
then
    export JENKINS_URL="http://localhost:8090"
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

if [ -f "$HOME/.nix-profile/share/bash-completion/completions/nix" ]
then
    . "$HOME/.nix-profile/share/bash-completion/completions/nix"
fi

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

alias t5='TMOUT=5'

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
         --manifest="$HOME/src/dotfiles/guix/manifests/oleg.scm"
}

guix-apply-my-manifest()
{
    guix package --substitute-urls='https://ci.guix.info http://cuirass.tld' \
         --fallback  --manifest="$HOME/src/dotfiles/guix/manifests/oleg.scm"
}

grub-list-entries()
{
    awk -F\" '$1=="menuentry " {print i++ " : " $2}' /boot/grub/grub.cfg
}

guix-package-show()
{
    guix package --search="$1" | recsel -e "name=\"$1\""
}

# https://github.com/jarun/Buku/wiki/Third-party-integration
fb()
{
    # save newline separated string into an array
    mapfile -t website <<< "$(buku -p -f 4 | column -ts$'\t' | fzf --multi)"

    # open each website
    for i in "${website[@]}"; do
        index="$(echo "$i" | awk '{print $1}')"
        buku -p "$index"
        buku -o "$index"
    done
}

guix-vm()
{
    script="$1"

    # https://wiki.archlinux.org/index.php/QEMU#Creating_bridge_manually
    printf -v macaddr "52:54:%02x:%02x:%02x:%02x" \
           $(( $RANDOM & 0xff)) \
           $(( $RANDOM & 0xff )) \
           $(( $RANDOM & 0xff)) \
           $(( $RANDOM & 0xff ))

    /run/setuid-programs/sudo \
        "$script" -daemonize \
        -smp cores=4,threads=1 -enable-kvm -cpu host \
        -m 4096 \
        -net nic,model=virtio,macaddr="$macaddr" -net bridge,br=br0 \
        -vga virtio -full-screen
}

myreconfigure-boot ()
{
    sudo -i guix system reconfigure \
         --load-path="$HOME/src/dotfiles/fiore/modules" \
         --substitute-urls='https://ci.guix.info http://cuirass.tld' \
         "$HOME/src/dotfiles/guixsd/guixsd.scm"
}

myreconfigure ()
{
    sudo -i guix system reconfigure --no-bootloader \
         --load-path="$HOME/src/dotfiles/fiore/modules" \
         --substitute-urls='https://ci.guix.info http://cuirass.tld' \
         --fallback "$HOME/src/dotfiles/guixsd/guixsd.scm" $@
}

myprereconfigure()
{
    sudo -i GUIX_PACKAGE_PATH="/home/oleg/src/guix-wigust/guix" \
            /home/oleg/src/guix/pre-inst-env \
            guix system reconfigure \
            --load-path="/home/oleg/src/dotfiles/fiore/modules" \
            --substitute-urls='https://ci.guix.info http://cuirass.tld' \
            --fallback "/home/oleg/src/dotfiles/guixsd/guixsd.scm"
}

mypull()
{
    guix pull --substitute-urls='https://ci.guix.info http://cuirass.tld' \
            --channels="$HOME/src/dotfiles/channels.scm"
}

mypull-sudo()
{
    sudo -i guix pull --channels="$HOME/src/dotfiles/channels.scm"
}

guix-master-staging()
{
    git -C "$HOME/src/guix" \
        shortlog -n upstream/master..upstream/staging
}

listen-ports()
{
    ss -tulpn | awk '{ print $5 }' | cut -d: -f 2 | sort -un | xargs echo
}

alias vnc-server-android="vncserver -AcceptSetDesktopSize=no -geometry 1280x720"
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"

ssh-root()
{
    ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -l root $@
}

sshpass-root ()
{
    password="$1"
    ip="$2"
    sshpass -p "$pass" ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -l root "$ip"
}

# jenkins-log()
# {
#     for project in $(curl -s -k 'https://admin:***REMOVED***@jenkins.intr/api/json?pretty=true' | jq -r '.jobs[] | .name'); do
#         for job in $(curl -s -k "https://admin:***REMOVED***@jenkins.intr/job/$project/api/json" | jq -r '.jobs[] | .url'); do
#             echo "@ $job" |
#             curl -u 'admin:***REMOVED***' -s -k "$job/job/master/lastBuild/consoleText"
#         done
#     done
# }

jenkins-log()
{
    for project in $(curl -s -k 'https://admin:***REMOVED***@jenkins.intr/api/json?pretty=true' | jq -r '.jobs[] | .name'); do
        mkdir -p "$project"
        cd "$project"
        for job in $(curl -s -k "https://admin:***REMOVED***@jenkins.intr/job/$project/api/json" | jq -r '.jobs[] | .url'); do
            job_name="$(echo $job | rev | cut -d/ -f 2 | rev)"
            echo "@ $job"
            curl -u 'admin:***REMOVED***' -s -k "$job/job/master/lastBuild/consoleText" > "$job_name.log"
        done
        cd -
    done
}

majordomo-add-hosts-mikrotik()
{
    # Add hosts from Majordomo to MikroTik.
    for host in $@; do
        ssh mikrotik -- /ip dns static add address="$(ssh majordomo -- dig +short a $host.intr 2>/dev/null)" name="$host.intr";
    done
}


xq-br()
{
    ssh -l root br1-mr14.intr -- 'cli -c "show interfaces | display xml"' \
        | xq -y '."rpc-reply"."interface-information"."physical-interface"[] | ."logical-interface" | select(. != null)'
}

juneos-config()
{
    sshpass -p***REMOVED*** ssh -l root "$1" -- 'cli -c "show config | display xml"'
}

# https://markhneedham.com/blog/2015/11/14/jq-filtering-missing-keys/

hms-current-stack()
{
    curl -u 'jenkins:***REMOVED***' -X GET http://nginx1.intr:8080/hms
}

ansible-docker-ps()
{
    ansible swarm -m shell -a 'docker ps' --become
}

docker_list_intr()
{
    curl -s -X GET -k -u 'gradle:***REMOVED***' https://docker-registry.intr/v2/_catalog \
        | jq -r '.repositories[]'
}

docker-pull-intr()
{
    group="$1" # For example: “mail”.
    for repo in $docker_list_intr; do
        docker pull "docker-registry.intr/$repo"
    done
}

jenkins-log()
{
    for project in $(curl -s -k 'https://admin:***REMOVED***@jenkins.intr/api/json?pretty=true' | jq -r '.jobs[] | .name'); do
        mkdir -p "$project"
        cd "$project"
        for job in $(curl -s -k "https://admin:***REMOVED***@jenkins.intr/job/$project/api/json" | jq -r '.jobs[] | .url'); do
            job_name="$(echo $job | rev | cut -d/ -f 2 | rev)"
            echo "@ $job"
            curl -u 'admin:***REMOVED***' -s -k "$job/job/master/lastBuild/consoleText" > "$job_name.log"
        done
        cd -
    done
}

ansible-swarm-ps-inspect()
{
    ansible swarm -m shell -a 'for c in $(docker ps | grep -v CONTAINER | cut -d " " -f 1 | xargs echo); do docker inspect $c; done' --become
}

ansible-swarm-network-inspect()
{
    ansible swarm -m shell -a 'docker network ls | cut -d " " -f 1 | grep -v NETWORK | xargs docker network inspect' --become
}

br1-mr14.intr()
{
    sshpass -p***REMOVED*** ssh -l root br1-mr14.intr
}

br1-mr14.intr-ftp-list()
{
    curl 'ftp://netcfg:***REMOVED***@172.16.103.111/junos/'
}

br1-mr14.intr-ftp()
{
    # Example “config”: br1-mr14.intr_juniper.conf.gz_20190702_170649
    config="$1"
    wget -O- "ftp://netcfg:***REMOVED***@172.16.103.111/junos/$config" | zcat
}

es-xmlrpc()
{
    curl -H 'Content-Type: application/json' \
         -X POST "http://es.intr:9200/nginx-$(date +"%Y.%m.%d")/_search/" \
         --data-binary '{"from":0,"query":{"query_string":{"query":"path.keyword:\"/xmlrpc.php\""}},"size":50,"sort":[{"@timestamp":{"order":"desc"}}]}'
}

hms-auth ()
{
    curl --silent \
         --request POST https://api.majordomo.ru/oauth/token \
         --header 'content-type: application/x-www-form-urlencoded' \
         --header 'x-requested-with: XMLHttpRequest' \
         -d "grant_type=password&username=$IHS_USER&password=$IHS_PASS&client_id=service&client_secret=service_secret" \
        | jq -r '.access_token'
}

kvm-test-delete-service()
{
    curl "https://api.majordomo.ru/rc-staff/service/$1" \
            -X DELETE --compressed \
            -H 'Content-type: application/json' \
            -H "Authorization: Bearer $(hms-auth)"
}

hms-fetch-services()
{
    for page in $(seq 1 50); do
        curl -s "https://api.majordomo.ru/rc-staff/service?page=$page&sort=name%2Casc&size=20" \
--compressed \
-H 'Content-type: application/json' \
-H 'X-HMS-Pageable: true' \
-H "Authorization: Bearer $(hms-auth)" \
-H 'Connection: keep-alive'
    done
}

kvm-test-delete-services()
{
    input="$1"
    for service in $(for version in 4 72; do
                         grep -B 1 "apache2-php$version-.*@kvm-test" "$input" \
                             | grep id \
                             | cut -d: -f 2 \
                             | cut -d, -f 1 \
                             | sed 's/"//g';
                     done); do
        echo "@ $service"
        curl "https://api.majordomo.ru/rc-staff/service/$service" \
-X DELETE --compressed \
-H 'Content-type: application/json' \
-H "Authorization: Bearer $(hms-auth)"
    done
}

kvm-test-delete-sockets()
{
    for socket in $(for version in 4; do
                        grep -B 1 "apache2-php$version-.*@kvm-test" out.json \
                            | grep id \
                            | cut -d: -f 2 \
                            | cut -d, -f 1 \
                            | sed 's/"//g'
                    done \
                        | xargs echo); do
        echo "@ $socket"
        curl "https://api.majordomo.ru/rc-staff/service-socket/$socket" \
-X DELETE --compressed -H 'Content-type: application/json' -H "Authorization: Bearer $(hms-auth)"
    done
}

docker-jenkins()
{
    docker -H ssh://dh4-mr.intr exec -it 4649529fa34d $@
}
nix-version() { nix-instantiate --eval -E '(import <nixpkgs> {}).lib.nixpkgsVersion'; }

alias dockerd='sudo herd start docker'
alias get-todos='scp work:/home/user/src/jord/doc/todo.org ~/src/todo.org '

# nix-shell -E 'with import <nixpkgs> {}; callPackage ./default.nix {}' -A luaCrypto
# nix-build -E 'with import <nixpkgs> {}; callPackage ./default.nix {}';

# grep '^FAIL ' /tmp/6.txt | cut -d '/' -f 2- | cut -d ']' -f 1 | sed 's@^@ext/@'

nix-untar-docker()
{
    # argument example: /nix/store/vqi100nf7x7z82pr4lkagasmzl9zj0zp-docker-image-apache2-php56.tar.gz
    tar xv --wildcards '*/layer.tar' -f "$1" \
        | tee /tmp/tempstore \
        | xargs -n1 -I{} tar xvf {}
}

docker-ps-ip()
{
    docker ps --format='{{.ID}}' \
        | xargs docker inspect \
        | jq -r '.[] | [.NetworkSettings.Networks.bridge.IPAddress, .Config.Image] | @tsv'
}

complete -C "$HOME/.nix-profile/bin/terraform" terraform

# dockerd --insecure-registry https://docker-registry.intr

nix-ls-store-kvm15()
{
    # $1 example: /nix/store/0i2jd68mp5g6h2sa5k9c85rb80sn8hi9-hello-2.10
    nix ls-store --store http://kvm15.intr:5556/ -lR "$1"
}

nix-build-kvm15()
{
    pkg="$1"
    nix-build build.nix --option  substituters http://kvm15.intr:5556/ --cores 4 -A nixpkgsUnstable$pkg --keep-going --keep-failed $@
}

tmux-renumber-windows()
{
    tmux movew -r
}

nix-user-uid()
{
    nix-build -E 'with import <nixpkgs> {}; runCommand "foo" {} "id"'
}

nix-info()
{
    nix-shell -p nix-info --run "nix-info -m"
}

print-environ()
{
    cat "/proc/$1/environ" | tr '\000' '\n'
}

guix-packages-json()
{
    curl https://guix.gnu.org/packages.json
}

skopeo-mj()
{
    image="$1" # ssh-guest-room
    tar="$2" || result # docker-archive:/nix/store/dw0qakl4g58n9idsi35vn0m1d92gs0jw-docker-image-ssh-guest-room.tar.gz
    skopeo copy --dest-creds=gradle:***REMOVED*** --dest-tls-verify=false "docker-archive:$tar" "docker://docker-registry.intr/webservices/$image:master"
}

skopeo-fetch()
{
    image="$1"
    dest="$2"
    skopeo copy --dest-creds=gradle:***REMOVED*** --src-tls-verify=false --dest-tls-verify=false "docker://docker-registry.intr/$image" "docker-archive:$dest"
}

git-guix-pre-new-build()
{
    number="$1"
    git log --oneline \
        | head -n "$number" \
        | grep Add  \
        | awk '{ print $NF }' \
        | cut -d'.' -f 1 \
        | xargs ./pre-inst-env guix build --no-grafts
}

git-guix-pre-new-lint()
{
    number="$1"
    git log --oneline \
        | head -n "$number" \
        | grep Add  \
        | awk '{ print $NF }' \
        | cut -d'.' -f 1 \
        | xargs ./pre-inst-env guix lint
}

git-guix-pre-update()
{
    number="$1"
    git log --oneline \
        | head -n "$number" \
        | grep Update \
        | awk '{ print $3 }' \
        | cut -d: -f 1 \
        | xargs ./pre-inst-env guix build --no-grafts
}

git-guix-home()
{
    number="$1"
    git show "$number" | grep home | cut -d'"' -f 2
}

nix-build-my()
{
    nix-build --no-out-link '<nixpkgs>' -A $@
}

archive()
{
    for dir in $@; do
        mv -vi "$dir" /home/oleg/archive/src/
    done
}

terraform-init-with-nix()
{
    terraform init -plugin-dir ~/.nix-profile/bin $@
}

terraform-init()
{
    terraform-init-with-nix -plugin-dir ~/go/src/gitlab.intr/majordomo/terraform-provider-majordomo
}

alias tsw='tmuxifier s web'
alias nn='notmuch new'
alias wtr='curl -H "Accept-Language: ru" wttr.in/Санкт-Петербург'
alias hylang='docker run --rm -it hylang'

alias nix-build-unstable="nix-build $HOME/.nix-defexpr/channels/nixos-unstable"

nix-shell-python()
{
    PYTHONPATH="" nix-shell -p "python35.withPackages(ps: with ps; [ $@ ])"
}

nixos-interactive-test()
{
    version="$1"
    nix-build build.nix \
              --cores 4 \
              -A "nixpkgsUnstable.php$version-test.driver" \
              --no-out-link --show-trace/bin/nixos-run-vms
}

terraform-refresh()
{
    NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
            terraform refresh
}

terraform-plan()
{
    NIX_SSL_CERT_FILE="$HOME/majordomo/office/ssl-certificates/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
            terraform plan -out=plan $@
}

terraform-import()
{
    NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
            terraform import $@
}

terraform-apply()
{
    NIX_SSL_CERT_FILE="$HOME/majordomo/office/ssl-certificates/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
                     terraform apply "plan"
}

terraform-apply-no-plan()
{
    NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
            terraform apply
}

terraform-apply-gitlab()
{
    grep resource gitlab/*.tf \
        | awk '{ gsub("\"",""); print $2, $3 }' \
        | sed 's/\s/./' \
        | sed 's/^/-target=/' \
        | xargs env NIX_SSL_CERT_FILE="/run/current-system/profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" SSL_CERT_DIR="/run/current-system/profile/etc/ssl/certs" SSL_CERT_FILE="/run/current-system/profile/etc/ssl/certs/ca-certificates.crt" terraform apply plan
}

terraform-plan-gitlab()
{
    grep resource gitlab/*.tf \
        | awk '{ gsub("\"",""); print $2, $3 }' \
        | sed 's/\s/./' \
        | sed 's/^/-target=/' \
        | xargs env NIX_SSL_CERT_FILE="/run/current-system/profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" SSL_CERT_DIR="/run/current-system/profile/etc/ssl/certs" SSL_CERT_FILE="/run/current-system/profile/etc/ssl/certs/ca-certificates.crt" terraform plan -out=plan
}

terraformer-import-github()
{
    terraformer import majordomo --token $GITHUB_TOKEN --organizations wugi-emacs --resources=repositories
}

spb-log()
{
    ssh spb -- bzcat "$1"
}

ssh-guix-build-log-file()
{
    host="$1"
    file="$2"
    ssh "$host" -- guix build --log-file "$file" | xargs ssh "$host" -- bzcat
}

web()
{
    tmux at -t web
}

alerta-top()
{
    ssh -t work -- 'while true; do .local/bin/alerta top; done'
}

herd-reload()
{
    herd reload root "$HOME/.config/shepherd/init.scm"
}

herd-kill-services-via-awk()
{
    for service in $(awk '/define.*service/ { print $NF }' "$HOME/.config/shepherd/init.scm" | sed 's/-service//'); do
        pkill "$service"
    done
}

web-active-current()
{
    curl -H "PRIVATE-TOKEN: $(pass show majordomo/gitlab.intr/tokens/terraform)" -s -k -L \
            'https://gitlab.intr/hms/config-repo/raw/master/rc-staff-prod.yml'
}

docker-strace()
{
    container="$1"
    docker top "$container" | tail -n +2 | awk '{ print $2 }' | sed 's/^/-p/' | xargs strace -o "~/$container.strace" -f -s 4096
}

docker-strace-pids()
{
    container="$1"
    docker top "$container" | tail -n +2 | awk '{ print $2 }' | sed 's/^/-p/';
}

vnc-server-zero()
{
    while true; do
        x0vncserver -PasswordFile "$HOME/.vnc/passwd" -display :0 -rfbport 5960
        sleep 5
    done
}

check-sites-on-ip()
{
    ip=$1
    domains=$@
    for domain in $domains; do
        d=$(idn2 $domain)
        printf "$domain "
        curl -o /dev/null -s -w "%{http_code}" -k -H "Host: $d" https://$ip/
        echo
    done
}

ansible-auth-hosts()
{
    for host in $(ansible all --list-hosts |grep intr); do
        echo -e "\n@ $host"
        ssh -oStrictHostKeyChecking=no $host -- uptime
    done
}

ansible-auth-hosts-sup()
{
    ansible sup --list-hosts
    for host in $(ansible sup --list-hosts | tail -n +2); do
        echo -e "\n@ $host"
        ssh  -p 1022 -l sup -i .ssh/id_rsa_sup_sup -oStrictHostKeyChecking=no $host -- uptime
    done
}

ansible-cmdb-my()
{
    # https://itnext.io/create-a-host-inventory-in-a-minute-with-ansible-c7bf251166d9
    ansible -m setup --tree out/ all
    ansible-cmdb -t html_fancy_split -p local_js=1 out/
}

bash-history-top()
{
    # https://www.commandlinefu.com/commands/view/604/list-of-commands-you-use-most-often
    history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
}

fast-scan()
{
    parallel -j200% -n1 -a textfile-with-hosts.txt nc -vz {} ::: 22
}

alias guix-docker-image-minimal='guix pack -f docker --symlink=/bin=bin bash'
alias find-yml="find -maxdepth 2 -name '*.yml' | grep -vF '.travis.yml' | grep -vF '.gitlab'"
alias docker-describe-image='dive'
alias inxi='inxi --full'
alias ansible-playbook-ping-all="ansible-playbook <(echo -e '---\n- hosts: all\n  tasks:\n    - ping:')"
alias ansible-playbook-cache-all="ansible-playbook <(echo -e '---\n- hosts:\n  - all\n  gather_facts: True\n')"
alias clock='while sleep 1;do tput sc;tput cup 0 $(($(tput cols)-29));date;tput rc;done &'
alias type-like-movie='echo "You can simulate on-screen typing just like in the movies" | pv -qL 10'
alias top-by-memory='ps aux | sort -nk +4 | tail'
alias smtpd='python -m smtpd -n -c DebuggingServer localhost:1025'
alias biggest='du -s * | sort -n | tail'
alias colors='for code in {0..255}; do echo -e "\\e[38;05;${code}m $code: Test"; done'
alias ps-tree='ps awwfux | less -S'
alias share='script -qf | tee >(nc -kl 5000) >(nc -kl 5001) >(nc -kl 5002)'
alias internet-programs='lsof -P -i -n | cut -f 1 -d " "| uniq | tail -n +2'

md()
{
    mkdir -p "$@" && cd "$@"
}

backup-mysql()
{
    for I in $(mysql -e 'show databases' -s --skip-column-names); do mysqldump $I | gzip > "$I.sql.gz"; done
}

kernel-graph()
{
    lsmod \
        | perl -e 'print "digraph \"lsmod\" {";<>;while(<>){@_=split/\s+/; print "\"$_[0]\" -> \"$_\"\n" for split/,/,$_[3]}print "}"' \
        | dot -Tpng \
        | feh -
}

netstat-graph()
{
    netstat -an \
        | grep ESTABLISHED \
        | awk '{print $5}' \
        | awk -F: '{print $1}' \
        | sort \
        | uniq -c \
        | awk '{ printf("%s\t%s\t",$2,$1) ; for (i = 0; i < $1; i++) {printf("*")}; print "" }'
}

random-password()
{
    length="$1"
    strings /dev/urandom | grep -o '[[:alnum:]]' | head -n "$length" | tr -d '\n'
}

# watch -n 1 mysqladmin --user=<user> --password=<password> processlist

# TODO:
# ansible-galera()
# {
#     ansible "galera$1.intr" -m copy -a "src=galera$1/mariadb-bin.0029$2 dest=/home/mariadb/mariadb-bin.0029$2" --become && ansible "galera$1.intr" -m file -a "path=/home/mariadb/mariadb-bin.0029$2 owner=mysql group=mysql" --become
# }

alias alexa-top="curl -qsSl http://s3.amazonaws.com/alexa-static/top-1m.csv.zip 2>/dev/null | zcat | grep .de | head -1000 | awk -F, '{print }'"

dnsperf-my()
{
    # https://muff.kiev.ua/content/dnsperf-testirovanie-proizvoditelnosti-dns-servera
    sudo dnsperf -d ~/Downloads/dnsperf-example.txt -s 127.0.0.1 -l 60
}


tmuxifier-webs-user()
{
    account="$(ihs web unix $1)"
    TMUXIFIER_USER=$(echo "$account" | recsel -P name) TMUXIFIER_HOST=$(echo "$account" | recsel -P server_name)s tmuxifier s ssh
}

tmuxifier-web()
{
    web="$1"
    TMUXIFIER_USER=root TMUXIFIER_HOST="web$web" tmuxifier w ssh-sudo
}

alias web=tmuxifier-web

tmuxifier-connect-host()
{
    host="$1"
    TMUXIFIER_USER=root TMUXIFIER_HOST="$host" tmuxifier w ssh-sudo
}

git-clean-up()
{
    for dir in apache2-php52 apache2-php53 apache2-php54 apache2-php55 apache2-php56 apache2-php70 apache2-php71 apache2-php72 apache2-php73 ; do
        cd $dir
        for branch in $(git branch -r | grep -v master | sed 's|origin/||'); do
            git push origin --delete $branch
        done
        cd -
    done
}

tmux-ls()
{
    tmux ls | cut -d ':' -f 1;
}

tmux-fzf()
{
    session="$1"
    tmux at -t "$(tmux-ls | fzf)"
}

wp-cron()
{
    dir="$1"
    nice -n 19 ionice -c2 -n7 find /home/u12345 -type f -name wp-cron.php | xargs -n1 dirname | xargs -n1 -I{} sh -c "echo -n '{} ';grep -rl {} /etc/nginx/sites-available | xargs awk -F'-' '\$1~/proxy_pass/ {print \$2}' | uniq" | awk '{print "* * * * * /opt/"$NF"/bin/php",$(NF-1)"/wp-cron.php"}'
}


jenkins-build-php()
{
    branch="$1"
    for job in apache2-php52 apache2-php53 apache2-php54 apache2-php55 apache2-php56 apache2-php70 apache2-php71 apache2-php72 apache2-php73 apache2-php74; do
        echo -e "\n@ $job"
        curl -u 'admin:***REMOVED***' -s -k \
"https://jenkins.intr/job/webservices/job/$job/job/$branch/build?delay=0sec" \
-H 'Content-type: application/x-www-form-urlencoded; charset=UTF-8' --data ''
        sleep 0.5
    done
}

nixpkgs-fix()
{
    for file in $(find ~/majordomo/_ci/nixpkgs* -type f -name '*.nix'); do
        echo -e "\n@ $file"
        sed -i 's|https://gitlab.intr/pyhalov/php52-extra.git|file:///home/oleg/majordomo/pyhalov/php52-extra|g' "$file"
        sed -i 's|git@gitlab.intr:shared/http_errors.git|/home/oleg/majordomo/shared/http_errors|g' "$file"
        sed -i 's|git@gitlab.intr:|file:///home/oleg/majordomo/|g' "$file"
        sed -i 's|https://gitlab.intr/|file:///home/oleg/majordomo/|g' "$file"
    done

    for file in $(find . -type f -name '*.nix'); do
        echo -e "\n@ $file"
        sed -i 's|(builtins.fetchGit { url = "git@gitlab.intr:_ci/nixpkgs.git"; ref = ".*"; })|/home/oleg/majordomo/_ci/nixpkgs|g' "$file"
    done
}

rm-emacs-backups()
{
    find . -type f -name '*~' -exec rm {} +
}

galera-df-home()
{
    for n in 1 2 3; do
        echo -e "\n@ galera$n.intr"
        ssh "galera$n.intr" -- df -h /home
    done
}

router.majordomo.ru()
{
    ssh -t work
    # sshpass -p$(pass show majordomo/router.majordomo.ru) ssh -vvv -oKexAlgorithms=+diffie-hellman-group1-sha1 -oHostKeyAlgorithms=+ssh-dss -p 1022 -l root -i ~/.ssh/eng_key_rsa router.majordomo.ru
}

majordomo-backup-mount()
{
    sudo -u majordomo-ssh-tunnel restic -r /srv/backup/majordomo mount /mnt/backup
}

alias xclipp='xclip -selection secondary'

docker-xorg()
{
    xhost +local:

    # MAYBE:
    # --device /dev/video0 \

    docker run -it \
            -v /tmp/.X11-unix:/tmp/.X11-unix \
            -e DISPLAY \
            --device /dev/dri \
            --device /dev/snd \
            -v /etc/localtime:/etc/localtime:ro \
            --device /dev/input \
            $@
}

obs-docker-setup()
{
    # https://github.com/mviereck/x11docker/wiki/Container-sound:-ALSA-or-Pulseaudio

    $(guix build pulseaudio)/bin/pactl load-module module-native-protocol-unix socket=/tmp/pulseaudio.socket
    cat > /tmp/pulseaudio.client.conf << EOF
default-server = unix:/tmp/pulseaudio.socket
# Prevent a server running in the container

autospawn = no
daemon-binary = /bin/true
# Prevent the use of shared memory

enable-shm = false
EOF
}

obs-docker()
{
    docker-xorg \
        --name obs \
        --rm \
        --env PULSE_SERVER=unix:/tmp/pulseaudio.socket \
        --env PULSE_COOKIE=/tmp/pulseaudio.cookie \
        --volume /tmp/pulseaudio.socket:/tmp/pulseaudio.socket \
        --volume /tmp/pulseaudio.client.conf:/etc/pulse/client.conf \
        --volume /home/oleg/obs:/home/obs \
        --volume /home/oleg/.Xauthority:/home/oleg/.Xauthority \
        --volume /srv/music/mp3:/home/obs/music \
        obs
}

obs-nix-setup()
{
    file="/run/opengl-driver"
    if [ -f "$file" ]
    then
        sudo ln -s "$(nix-build-my vaapiIntel)" "$file"
    else
        echo "File $file already exists."
    fi

}

obs-nix()
{
    nixGLIntel obs "$@"
}

projectile-ls()
{
    bash -c 'echo ${0:1:-1}' \
         "$(printf "%b" $(emacsclient -e "(mapconcat 'identity (mapcar #'expand-file-name (projectile-load-known-projects)) \"\n\")"))" \
        | tr ' ' '\n'
}

projectile-command()
{
    "$@" "$(projectile-ls | fzf)"
}

alias projectile-cd="projectile-command cd"

alias projectile-magit="projectile-command magit"

microseconds-to-seconds()
{
    seconds="$1"
    microseconds=$(echo "scale=2;${seconds}/1000000" | bc)
    echo $microseconds
}

test-openvpn()
{
    server="$1"
    # https://serverfault.com/questions/262474/how-to-check-that-an-openvpn-server-is-listening-on-a-remote-port-without-using
    echo -e "\x38\x01\x00\x00\x00\x00\x00\x00\x00" |
        timeout 10 nc -u "$server" 1194 | cat -v
    # Output example: @$M-^HM--LdM-t|M-^X^@^@^@^@^@@$M-^HM--LdM-t|M-^X^@^@^@^@^@@$M-^HM--LdM-t|M-^X...
}

# jq like for http
alias hq='pup'

guix-graph-chromium()
{
    package="$1"
    GUILE_LOAD_PATH=$HOME/src/guix:GUILE_LOAD_PATH guix graph -b d3js \
"$package" > /tmp/out.html && chromium --app=file:///tmp/out.html
}

docker-run-ansible()
{
    docker run \
            --network=host \
            -v /home/oleg/src/dotfiles:/root/src/dotfiles \
            -v /root/.ssh:/root/.ssh \
            -v /home/oleg/.ansible-hosts:/etc/ansible/hosts \
            -v /home/oleg/telnet.yml:/telnet.yml \
            -v /home/oleg/ansible-out/ansible.cfg:/etc/ansible/ansible.cfg \
            --rm -it quay.io/ansible/molecule:2.22 sh
}

vnc-kvm()
{
    vncviewer "kvm$1":$(( 6017 + 5900 ))
}

assh-hosts()
{
    assh config json | jq '.hosts | keys'
}

alias bash-pure='env -i "$(command -v bash)" --login --noprofile --norc'

alias root-shedule="sudo herd schedule mcron 10"
alias tmux-reload="tmux source-file ~/.tmux.conf"

mj-vpn-ssh()
{
    sshuttle -r majordomo 10.0.0.0/8 172.16.0.0/16
}

less-color()
{
    export LESSOPEN="| ~/.guix-profile/bin/src-hilite-lesspipe.sh %s"
    export LESS=' -R '
}

dockerfile-lint()
{
    docker run --rm -i hadolint/hadolint:v1.17.2-8-g65736cb-debian < Dockerfile
}

ip-to-decimal()
{
    ip="$1" # e.g. 127.0.0.1
    perl -le "print unpack(\"N\", $ip)"
}

alias random-pass="perl -le 'print map { (a..z)[rand 26] } 1..8'"

urlescape ()
{
    perl -MURI::Escape -lne 'print uri_escape($_)' <<< "$1"
}

urlunescape ()
{
    perl -MURI::Escape -lne 'print uri_unescape($_)' <<< "$1"
}

build-farm()
{
    emacsclient -e "(wi-build-farm \"$1\")"
}

find-touch-go()
{
    find . -name '*.go' -exec touch {} +
}

pass-show-fzf()
{
    (
        cd ~/.password-store || exit
        pass show "$(find . -not -path './.gitattributes' -not -path './.git/*' -type f | sed 's@\./@@' | sed 's@\.gpg@@' | fzf)"
    )
}

pass-list-all()
{
    (
        cd ~/.password-store || exit
        for password in $(find . -not -path './.gitattributes' -not -path './.git/*' -type f | sed 's@\./@@' | sed 's@\.gpg@@'); do
            pass show "$password" | sed '/^$/d'
        done
    )
}

gmail-mail()
{
    # Source: https://www.commandlinefu.com/commands/view/3380/check-your-unread-gmail-from-the-command-line
    curl -u "go.wigust:$(pass show email/gmail/go.wigust)" \
            --silent "https://mail.google.com/mail/feed/atom" \
        | tr -d '\n' \
        | awk -F '<entry>' '{for (i=2; i<=NF; i++) {print $i}}' \
        | sed -n "s/<title>\(.*\)<\/title.*name>\(.*\)<\/name>.*/\2 - \1/p"

    # Alternative variant:
    # Checks your unread Gmail from the command line
    # curl -u username --silent "https://mail.google.com/mail/feed/atom" | perl -ne 'print "\t" if /<name>/; print "$2\n" if /<(title|name)>(.*)<\/\1>/;

}

gmail-send()
{
    rcpt="$1"

    # Send email with curl and gmail
    curl -n --ssl-reqd --mail-from "<go.wigust@gmail.com>" --mail-rcpt "$rcpt" --url smtps://smtp.gmail.com:465 -T file.txt
}

cmdfu()
{
    curl -L "http://www.commandlinefu.com/commands/matching/$@/$(echo -n $@ | openssl base64)/plaintext";
}

clfavs()
{
    #  backup all your commandlinefu.com favourites to a plaintext file 

    URL="http://www.commandlinefu.com"
    wget -O - --save-cookies c --post-data "username=$1&password=$2&submit=Let+me+in" $URL/users/signin
    for i in $(seq 0 25 "$3"); do
        wget -O - --load-cookies c "$URL/commands/favourites/plaintext/$i" >> "$4"
    done
    rm -f c
}

gnuplot-bash-history()
{
    HISTTIMEFORMAT='' history \
        | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' \
        | sort -rn \
        | head > /tmp/cmds ; gnuplot -persist <<<'plot "/tmp/cmds" using 1:xticlabels(2) with boxes'
}

kill-xterm-on-display()
{
    display="$1"
    for pid in $(pidof xterm); do
        if [[ $1 = $(tr '\000' '\n' < "/proc/$pid/environ" | grep "$display" | cut -d= -f 2) ]]; then
            kill "$pid"
        fi
    done
}

alias dmesg="sudo dmesg -T|sed -e 's|\(^.*'`date +%Y`']\)\(.*\)|\x1b[0;34m\1\x1b[0m - \2|g'"

docker-top-strace()
{
    container="$1"
    docker top "$container" \
        | awk '{ print $2 }' \
        | tail -n +2 \
        | sed 's/^/-p/' \
        | xargs strace -s 10000 -f -o /tmp/docker.strace
}

alias active-hms='curl -s --user jenkins:***REMOVED*** nginx{1,2}-mr:8080/hms | jq -r .active | uniq'

guix-pull-commit-me-and-root()
{
    commit="$1"
    guix pull --commit="$commit"; sudo -i guix pull --commit="$commit"
}

jenkins-build-project-branch()
{
    dir="$1"
    project="$2"
    branch="$3"
    url="$JENKINS_URL/job/$dir/job/$project/job/$branch"

    curl -X POST                                        \
         -u "admin:$(pass show jenkins/admin-api-key)"  \
         "$url/build"

    echo "$url/lastBuild/console"
}

# Create a new directory and enter it
mkd()
{
	mkdir -p "$@"
	cd "$@" || exit
}

# Make a temporary directory and enter it
tmpd()
{
	local dir
	if [ $# -eq 0 ]; then
		dir=$(mktemp -d)
	else
		dir=$(mktemp -d -t "${1}.XXXXXXXXXX")
	fi
	cd "$dir" || exit
}

# Run `dig` and display the most useful info
digga()
{
	dig +nocmd "$1" any +multiline +noall +answer
}

alias ff="feh --borderless --image-bg black --auto-zoom --draw-filename"
