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

if [ -f "$HOME/.guix-profile/etc/bash_completion.d/ihs" ]
then
    # Load the Bash aliases and functions.
    . "$HOME/.guix-profile/etc/bash_completion.d/ihs"
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
export IHS_USER='pyhalov'
export IHS_PASS='***REMOVED***'
export AWX_ADMIN_PASS='***REMOVED***'
export SUP_PASS='***REMOVED***'

ansible-host()
{
    ansible --inventory $1, $1 --become --ask-become-pass ${@:2}
}

ssh-sudo()
{
    ssh -t sup@$2 -- "sudo --stdin --validate --prompt='' <<< $JORD_PASS \
&& sudo --user=$1 --login ${@:3}" 2>/dev/null;
}

vm-ip()
{
    ihs vm ip $1 | recsel -Pip_address
}

jord-ansible-service-restart()
{
    # vm="$(gms vm ip $1 | recsel -pip_address | awk '{ print $2 }')"
    vm=$1
    ansible --user sup --private-key=$HOME/.ssh/id_rsa_sup --inventory $vm, \
            $vm --module-name service --args "name=$2 state=restarted" \
            --become --extra-vars="ansible_become_pass=***REMOVED*** ansible_ssh_common_args='-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'"

}

ssh-forward-vnc()
{
    ssh -i $HOME/.ssh/id_rsa_autossh -fNL 59551:localhost:5901 majordomo-ssh-tunnel@guix.duckdns.org
}

vm-ssh()
{
    ip="$(vm-ip $1)"
    echo "$ip"
    ssh -l sup -i $HOME/.ssh/id_rsa_sup "$ip"
}

jord-web-loadavg()
{
    watch --color "seq -f 'web%gs' 15 37 | grep -v web24s | xargs loadavg weather"
}

jord-web-account-check()
{
    parallel --will-cite -k 'printf "domain: %s\n" {1}; curl --max-time 10 -L -s -o /dev/null -w "ip-address: %{remote_ip}\nstatus_code: %{http_code}" {1}; printf "\n\n"' ::: $(gms account website $1 | recsel -pname | awk '{ print $2 }')
}

domain()
{
    domain="$1"
    ihs web search domain "$domain" | recsel -e "name=\"$domain\""
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
    grep --no-filename $pattern /mnt/backup/$host/snapshots/*/home/$user/.bash_history | sort -u
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

#eval "$(direnv hook bash)"
sup()
{
    host="$1"
    sshpass -p $JORD_PASS ssh sup@$host
}

export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"

top-mysql()
{
    web=$1
    ansible $web -m shell -a "mysql -h $web.majordomo.ru -s -u root -p***REMOVED*** -e \"show full processlist\"" --become | sort -k 6 -n
}

tmux-ls()
{
    tmux ls | cut -d ':' -f 1;
}

tmux-grep()
{
    tmux-ls | grep $@
}

tmux-fzf()
{
    session="$1"
    tmux at -t "$(tmux-ls | fzf)"
}

tmux-kill-sessions()
{
    parallel tmux kill-session -t {} ::: $(tmux-ls | grep 'vm\|^u')
}

get-host()
{
    getent hosts $1 | awk '{print $1}' | xargs host
}

pagespeed()
{
    echo "https://developers.google.com/speed/pagespeed/insights/?hl=ru&url=$url"
}

measure_get () {
    curl -L \
         -s \
         -o /dev/null \
         -w "url_effective: %{url_effective}\\n\
http_code: %{http_code}\\n\
num_connects: %{num_connects}\\n\
num_redirects: %{num_redirects}\\n\
remote_ip: %{remote_ip}\\n\
remote_port: %{remote_port}\\n\
size_download: %{size_download} B\\n\
speed_download: %{speed_download} B/s\\n\
time_appconnect: %{time_appconnect} s\\n\
time_connect: %{time_connect} s\\n\
time_namelookup: %{time_namelookup} s\\n\
time_pretransfer: %{time_pretransfer} s\\n\
time_redirect: %{time_redirect} s\\n\
time_starttransfer: %{time_starttransfer} s\\n\
time_total: %{time_total} s\\n" \
         "$1"
}

# TODO:
# ihs-connect()
# {
#     account="$1"
#     unix_account="$(ihs web unix $account)"
#     web_server="$(echo )"
# }

htrace.sh()
{
    host="$1"
    docker run --rm -it --name htrace.sh htrace.sh -d "$host" -s -h
}

guix-pull()
{
    guix pull --substitute-urls='http://cuirass.tld https://berlin.guixsd.org https://mirror.hydra.gnu.org'
}

we()
{
    account="$(ihs web unix $1)"
    ssh-sudo $(echo "$account" | recsel -P name) $(echo "$account" | recsel -P server_name)s
}

www()
{
    ssh-sudo root "$1"
}

web()
{
    account="$(ihs web unix $1)"
    TMUXIFIER_USER=$(echo "$account" | recsel -P name) TMUXIFIER_HOST=$(echo "$account" | recsel -P server_name)s tmuxifier s ssh
}

webs()
{
    account="$(ihs web unix $1)"
    ssh-sudo $(echo "$account" | recsel -P name) $(echo "$account" | recsel -P server_name)s
}

hms()
{
    ihs web open "$1"
}

jord-web-list()
{
    printf "http://%s\n" $(getent hosts $(ihs web website $@ | recsel -Pname) | awk '{ print $2 }')
}

# rsync --list-only rsync://sup@kvm34/vm17051
# ***REMOVED***

es()
{
    account="$1"
    curl -s -X GET "http://es:9200/logstash-apigw-*/_search" -H 'Content-Type: application/json' -d"{\"from\": 0, \"size\" : 1000, \"query\": {\"match\": {\"account_id\": $account}}}" | jq -r '.hits.hits[] | ._source | [."@timestamp", .account_id, .remote_addr] | @tsv'
}

es-list-indexes()
{
    curl -s -X GET "http://es:9200/_cat/indices?v" -H 'Content-Type: application/json'
}

mysql-json()
{
    mysql --pager='less -S' -u root -h web37 -p***REMOVED*** -e 'SELECT json_object("id", id, "user", user, "host", host, "db", db, "command", command, "time", time, "state", state, "time_ms", time_ms, "stage", stage, "
max_stage", max_stage, "progress", progress, "memory_used", memory_used, "max_memory_used", max_memory_used, "examined_rows", examined_rows, "query_id", query_id, "tid", tid) FROM information_schema.processlist ORDER BY Time' 2>/dev/null \
        | tail -n +2 | jq -c -s .
}

ansible-fetch-logs()
{
    for src in $@; do ansible web18s -m fetch -a "src=$src dest=/tmp" --become; done
}

goaccess-helper()
{
    zcat www.pravoved-centre.ru-access.log.*.gz | goaccess -o /tmp/output.html --log-format=COMBINED www.pravoved-centre.ru-access.log 
}

acc()
{
    emc "$(emacs-ihs $1; echo)"
}

# /usr/bin/python3 /usr/local/bin/gunicorn run:app -c /etc/gunicorn/gunicorn.conf

my()
{
    web="web$1"
    mysql -s -u root -h "$web" -p***REMOVED*** -e 'SELECT * FROM INFORMATION_SCHEMA.PROCESSLIST ORDER BY TIME_MS'
}

mytop-web()
{
  mytop -u root -h "web$1" -p***REMOVED*** "$@"
}

alias ip='ip --color'
alias ipba='ip --brief a'
alias ipbr='ip --brief r'

ban-all()
{
    parallel ~/bin/block-ip {} ::: $(seq -f 'web%g' 15 36 | grep -v web24) ::: "$1"
}

# Select concat('KILL ',id,';') from information_schema.processlist;

ansible-ddos()
{
    ANSIBLE_STDOUT_CALLBACK=json ansible-playbook "$HOME/ansible-list-http.yml" \
        | jq -r '.plays[].tasks[].hosts | .[].stdout' \
        | grep -v null \
        | sort -n \
        | sed 's/\s/\t/'
}

ansible-spam()
{
    ANSIBLE_STDOUT_CALLBACK=json ansible-playbook "$HOME/mail-list-spam.yml" \
        | jq -r '.plays[].tasks[].hosts | .[].stdout' \
        | grep -v null \
        | sort -n \
        | sed 's/\s/\t/' \
        | sed 's/@/\t/'
}

dns-zone()
{
    cat out | jq -j '.dnsResourceRecords[] | .ownerName, "."," ", .ttl, " ", .rrClass, " ", .rrType, " ",.data,"\n"' | head
}

vm-apache-logs()
{
    vm_ip="$(vm-ip $1)"
    ANSIBLE_STDOUT_CALLBACK=json ansible-playbook \
                           --user sup \
                           --private-key="$HOME/.ssh/id_rsa_sup" \
                           --extra-vars="ansible_become_pass=***REMOVED*** ansible_ssh_common_args='-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'" \
                           -i"$vm_ip", \
                           "$HOME/ansible-vm-apache-logs.yml" \
        | jq -r '.plays[].tasks[].hosts | .[].stdout' \
        | grep -v null \
        | cut -d ':' -f 2 \
        | sed -e 's/^[[:space:]]*//'
}

ns-test()
{
    set -x
    dig +short majordomo.ru @ns.majordomo.ru
    dig +short majordomo.ru @ns2.majordomo.ru
    dig +short majordomo.ru @ns3.majordomo.ru
    set +x
}

mydel()
{
    curl -XDELETE 'web32/ip-filter/81.95.28.29'
}

ssl-check()
{
    for host in $(seq -f 'web%g' 15 37 | grep -v 26 | grep -v 24); do echo "@ $host"; curl -I "https://$host.majordomo.ru/"; done
}

# ***REMOVED***

ansible-sup-fetch()
{
    vm_ip="$(vm-ip vm25846)"
    ansible \
        --user sup \
        --private-key="$HOME/.ssh/id_rsa_sup" \
        --extra-vars="ansible_become_pass=***REMOVED*** ansible_ssh_common_args='-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no'" \
        -i"$vm_ip", \
        $vm_ip \
        -m fetch \
        -a "src=/var/www/vm25846/data/www/ugimator.ru/app/Python/glove.6B/glove.6B.300d.txt dest=$HOME" \
        --become
}

awx-goaccess()
{
    host="$1"
    home="$2"
    site="$3"
    curl 'http://malscan:8052/api/v2/job_templates/10/launch/' -H 'Authorization: Bearer ***REMOVED***' -H 'Content-Type: application/json;charset=utf-8' --data "{\"extra_vars\":{\"host\":\"$host\",\"home\":\"$home\",\"site\":\"$site\"}}"
}

jord-docker-list()
{
    curl -s -X GET -k -u 'gradle:***REMOVED***' https://docker-registry.intr/v2/_catalog | jq
}

list-gitlab-intr()
{
    group="$1"

    # git user private token: ***REMOVED***
    curl -H 'PRIVATE-TOKEN: ***REMOVED***' \
         -H 'Content-Type: application/json' \
         -k \
         -X GET \
         "https://gitlab.intr/api/v4/projects/$group"
}

list-groups-gitlab-intr()
{
    curl -H 'PRIVATE-TOKEN: ***REMOVED***' \
            -H 'Content-Type: application/json' \
            -k -X GET "https://gitlab.intr/api/v4/groups/"
}


clone-gitlab-intr()
{
    # $1=hms/taskexecutor.git
    url="$1"
    group_and_repo=$(echo -n "$url" | sed "s@https://gitlab.intr/@@")
    git clone "https://git:***REMOVED***@gitlab.intr/$group_and_repo"
}


supeng-ssh()
{
    ip="$1"
    pass="$2"
    sshpass -p "$pass" ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -l root "$ip"
}

alias r1='supeng-ssh 78.108.89.188 ***REMOVED***'
alias r2='supeng-ssh 78.108.93.115 ***REMOVED***'
alias r3='supeng-ssh 78.108.90.34 ***REMOVED***'
alias r4='supeng-ssh 78.108.91.85 ***REMOVED***'

# alias r1='supeng-ssh 78.108.88.91 ***REMOVED***'
# alias r2='supeng-ssh 78.108.88.92 ***REMOVED***'
# alias r3='supeng-ssh 78.108.88.93 ***REMOVED***'
# alias r4='supeng-ssh 78.108.88.94 ***REMOVED***'

alias ping1='supeng-ssh 78.108.88.8 ***REMOVED***'
alias ping2='supeng-ssh 78.108.89.141 ***REMOVED***'

alias s1='supeng-ssh 78.108.88.240 ***REMOVED***'
alias s2='supeng-ssh 178.250.243.49 ***REMOVED***'

alias bitrix='supeng-ssh 178.250.246.138 ***REMOVED***'

alias vm26635='supeng-ssh 78.108.92.88 ***REMOVED***'
alias vm26638='supeng-ssh 78.108.89.228 ***REMOVED***'
alias vm26641='supeng-ssh 178.250.240.149 ***REMOVED***'

alias r5='supeng-ssh 178.250.243.152 ***REMOVED***'

export PATH="$PATH:$HOME/go/bin"

elktail-nginx()
{
    elktail --url http://es:9200 --index-pattern nginx-$(date +"%Y.%m.%d")
}

ssh-eng()
{
    ***REMOVED***
}


# mysql-cache()
# {
#     while :;do echo "select USER,DB,STATE from information_schema.processlist where STATE like '%cache%'" | mysql -N;done
# }

# my-tcpdump()
# {
#     sudo tcpdump -n -pi any $@
# }

# pass-route()
# {
#     echo ***REMOVED***
#     # PXE 254 и 253
# }

# apache '{ print $NF, $1, $6, $10, $11 }'

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

docker-pull-intr()
{
    group="$1" # For example: “mail”.
    for repo in $(curl -s -X GET -k -u 'gradle:***REMOVED***' https://docker-registry.intr/v2/_catalog \
                      | jq -r '.repositories[]' \
                      | grep "$group/"); do
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
