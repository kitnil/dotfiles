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
         --manifest="$HOME/src/dotfiles/guix/manifests/oleg.scm"
}

guix-apply-my-manifest()
{
    guix package --substitute-urls='https://ci.guix.info http://cuirass.tld' \
         --manifest="$HOME/src/dotfiles/guix/manifests/oleg.scm"
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
         "$HOME/src/dotfiles/guixsd/config.scm"
}

myreconfigure ()
{
    sudo -i guix system reconfigure --no-bootloader \
         --load-path="$HOME/src/dotfiles/fiore/modules" \
         --substitute-urls='https://ci.guix.info http://cuirass.tld' \
         "$HOME/src/dotfiles/guixsd/config.scm"
}

mypull()
{
    guix pull --channels="$HOME/src/dotfiles/channels.scm"
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

sshpass ()
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
        | jq -r '.repositories[]' \
        | grep "$group/"
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
                     TF_VAR_GITLAB_TOKEN=***REMOVED*** \
                     TF_VAR_MAJORDOMO_PASSWORD=***REMOVED*** \
                     TF_VAR_MAJORDOMO_USER=pyhalov \
            terraform refresh
}

terraform-plan()
{
    NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
                     TF_VAR_GITLAB_TOKEN=***REMOVED*** \
                     TF_VAR_MAJORDOMO_PASSWORD=***REMOVED*** \
                     TF_VAR_MAJORDOMO_USER=pyhalov \
            terraform plan -out=plan $@
}

terraform-import()
{
    NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
                     TF_VAR_GITLAB_TOKEN=***REMOVED*** \
                     TF_VAR_MAJORDOMO_PASSWORD=***REMOVED*** \
                     TF_VAR_MAJORDOMO_USER=pyhalov \
            terraform import $@
}

terraform-apply()
{
    NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
                     TF_VAR_GITLAB_TOKEN=***REMOVED*** \
                     TF_VAR_MAJORDOMO_PASSWORD=***REMOVED*** \
                     TF_VAR_MAJORDOMO_USER=pyhalov \
                     terraform apply "plan"
}

terraform-apply-no-plan()
{
    NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" \
                     SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" \
                     SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" \
                     TF_VAR_GITLAB_TOKEN=***REMOVED*** \
                     TF_VAR_MAJORDOMO_PASSWORD=***REMOVED*** \
                     TF_VAR_MAJORDOMO_USER=pyhalov \
            terraform apply
}

terraform-plan-gitlab()
{
    grep resource gitlab/*.tf \
        | awk '{ gsub("\"",""); print $2, $3 }' \
        | sed 's/\s/./' \
        | sed 's/^/-target=/' \
        | xargs env NIX_SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/Majordomo_LLC_Root_CA.crt" SSL_CERT_DIR="$HOME/.guix-profile/etc/ssl/certs" SSL_CERT_FILE="$HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt" TF_VAR_GITLAB_TOKEN=***REMOVED*** TF_VAR_MAJORDOMO_PASSWORD=***REMOVED*** TF_VAR_MAJORDOMO_USER=pyhalov terraform plan -out=plan
}

export GITHUB_TOKEN_TERRAFORMER=***REMOVED***

terraformer-import-github()
{
    terraformer import majordomo --token $GITHUB_TOKEN --organizations wugi-emacs --resources=repositories
}

ssh-guix-log-file()
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
    for service in $(awk '/define.*service/ { print $NF }' .config/shepherd/init.scm | sed 's/-service//'); do
        pkill "$service"
    done
}

web-active-current()
{
    curl -H 'PRIVATE-TOKEN: ***REMOVED***' -s -k -L \
            'https://gitlab.intr/hms/config-repo/raw/master/rc-staff-prod.yml'
}

docker-strace()
{
    container="$1"
    docker top "$container" | tail -n +2 | awk '{ print $2 }' | sed 's/^/-p/' | xargs strace -f -s 4096
}

docker-strace-pids()
{
    container="$1"
    docker top "$container" | tail -n +2 | awk '{ print $2 }' | sed 's/^/-p/';
}

vnc-server-zero()
{
    while true; do x0vncserver -PasswordFile .vnc/passwd -display :0 -rfbport 5960; sleep 5; done
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

ansible-cmdb-my()
{
    # https://itnext.io/create-a-host-inventory-in-a-minute-with-ansible-c7bf251166d9
    ansible -m setup --tree out/ all
    ansible-cmdb -t html_fancy_split -p local_js=1 out/
}

history-top()
{
    # https://www.commandlinefu.com/commands/view/604/list-of-commands-you-use-most-often
    history | awk '{a[$2]++}END{for(i in a){print a[i] " " i}}' | sort -rn | head
}

alias guix-docker-image-minimal='guix pack -f docker --symlink=/bin=bin bash'
alias find-yml="find -maxdepth 2 -name '*.yml' | grep -vF '.travis.yml' | grep -vF '.gitlab'"
alias docker-describe-image='dive'
alias ansible-playbook-ping-all="ansible-playbook <(echo -e '---\n- hosts: all\n  tasks:\n    - ping:')"
alias ansible-playbook-cache-all="ansible-playbook echo -e '---\n- hosts:\n  - all\n  gather_facts: True\n'"
alias clock='while sleep 1;do tput sc;tput cup 0 $(($(tput cols)-29));date;tput rc;done &'
alias type-like-movie='echo "You can simulate on-screen typing just like in the movies" | pv -qL 10'
alias top-by-memory='ps aux | sort -nk +4 | tail'
alias smtpd='python -m smtpd -n -c DebuggingServer localhost:1025'

# TODO:
# ansible-galera()
# {
#     ansible "galera$1.intr" -m copy -a "src=galera$1/mariadb-bin.0029$2 dest=/home/mariadb/mariadb-bin.0029$2" --become && ansible "galera$1.intr" -m file -a "path=/home/mariadb/mariadb-bin.0029$2 owner=mysql group=mysql" --become
# }
