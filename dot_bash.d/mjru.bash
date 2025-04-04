mjdev.intr()
{
    sshpass -p"$(pass show majordomo/public/mjdev.intr/root)" ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no root@mjdev.intr "$@"
}

mjru-backup()
{
    case "$1" in
        list)
            if [ -z "$2" ]; then
                echo "provide unix_account_name"
                echo "example: backup_list u168138"
                (exit 1)
            else curl -s bareos.intr/_snapshot/slice/"$(echo -n "$2" | sha1sum  |  cut -c -2)/$2 | jq -r '.[] | [.dataUri.rsync, .time] | @tsv'"
            fi
            ;;        
        mount)
            if [ -z "$2" ]; then
                echo "provide unix_account_name"
                echo "example: backup_mount u168138"
                (exit 1)
            else curl -s -XPOST  "bareos.intr/_mount/slice/$(echo -n "$2" | sha1sum  |  cut -c -2)/$2?wait=True&timeout=600" | jq -r
            fi
            ;;
        umount)
            if [ -z "$2" ]; then
                echo "provide unix_account_name"
                echo "example: backup_umount u168138"
                (exit 1)
            else curl -s -XDELETE  "bareos.intr/_mount/slice/$(echo -n "$2" | sha1sum  |  cut -c -2)/$2" | jq -r
            fi
            ;;
    esac
}

mjru-influx()
{
    case "$1" in
        list)
            curl -G "http://influx.intr:8086/query?pretty=true" --data-urlencode "q=show databases" | jq
            ;;
        series)
            curl -G "http://influx.intr:8086/query?db=telegraf&pretty=true" --data-urlencode "q=SHOW SERIES"
            ;;
    esac
}

mjru-sshuttle()
{
    sshuttle -r majordomo 10.0.0.0/8 172.16.0.0/16
}

mjru-vnc()
{
    vncviewer "kvm$1":$(( $2 + 5900 ))
}

mjru-galera-df-home()
{
    for n in 1 2 3; do
        echo -e "\n@ galera$n.intr"
        ssh "galera$n.intr" -- df -h /home
    done
}

router4.intr()
{
    sshpass -p"$(pass show majordomo/public/router4/root)" ssh router4.intr "$@"
}

router.majordomo.ru()
{
    ssh -t work
    # sshpass -p$(pass show majordomo/public/majordomo/router.majordomo.ru) ssh -vvv -oKexAlgorithms=+diffie-hellman-group1-sha1 -oHostKeyAlgorithms=+ssh-dss -p 1022 -l root -i ~/.ssh/id_rsa_majordomo_eng router.majordomo.ru
}

br1-mr14.intr-ftp-list()
{
    curl "ftp://netcfg:$(pass show majordomo/public/172.16.103.111/netcfg)@172.16.103.111/junos/"
}

br1-mr14.intr-ftp()
{
    # Example “config”: br1-mr14.intr_juniper.conf.gz_20190702_170649
    config="$1"
    wget -O- "ftp://netcfg:$(pass show majordomo/public/172.16.103.111/netcfg)@bareos.intr/junos/$config" | zcat
}

mjru-juneos-config()
{
    sshpass -p"$(pass show majordomo/public/majordomo/ssh/router)" ssh -l root "$1" -- 'cli -c "show config | display xml"'
}

mjru-br1-mr14.intr-xq-br()
{
    ssh -l root br1-mr14.intr -- 'cli -c "show interfaces | display xml"' \
        | xq -y '."rpc-reply"."interface-information"."physical-interface"[] | ."logical-interface" | select(. != null)'
}

mjru-backup-mount()
{
    sudo -u majordomo-ssh-tunnel restic -r /srv/backup/majordomo mount /mnt/backup
}

mjru-nix-repl()
{
    echo "overlay = lib.listToAttrs (map (drv: lib.nameValuePair drv.name drv) (import ./build.nix))"
}

mjru-nix-fix()
{
    while IFS= read -r -d '' file
    do
        echo -e "\n@ $file"
        sed -i 's|https://gitlab.intr/pyhalov/php52-extra.git|file:///home/oleg/src/gitlab.intr/pyhalov/php52-extra|g' "$file"
        sed -i 's|git@gitlab.intr:shared/http_errors.git|/home/oleg/src/gitlab.intr/shared/http_errors|g' "$file"
        sed -i 's|git@gitlab.intr:|file:///home/oleg/src/gitlab.intr/|g' "$file"
        sed -i 's|https://gitlab.intr/|file:///home/oleg/src/gitlab.intr/|g' "$file"
    done < <(find ~/src/gitlab.intr/_ci/nixpkgs* -type f -name '*.nix' -print0)

    while IFS= read -r -d '' file
    do
        echo -e "\n@ $file"
        sed -i 's|(builtins.fetchGit { url = "git@gitlab.intr:_ci/nixpkgs.git"; ref = ".*"; })|/home/oleg/src/gitlab.intr/_ci/nixpkgs|g' "$file"
        sed -i 's|(builtins.fetchGit { url = "git@gitlab.intr:_ci/nixpkgs.git"; inherit ref; })|/home/oleg/src/gitlab.intr/_ci/nixpkgs|g' "$file"
    done < <(find . -type f -name '*.nix' -print0)
}

mjru-jenkins-build-php()
{
    jobs=(
        apache2-php44
        apache2-php52
        apache2-php53
        apache2-php54
        apache2-php55
        apache2-php56
        apache2-php70
        apache2-php71
        apache2-php72
        apache2-php73
        apache2-php74
        apache2-php80
        apache2-php81
        apache2-php83
    )
    branch="$1"
    for job in "${jobs[@]}"; do
        echo -e "\n\n@ ${job}"
        /home/oleg/.guix-profile/bin/curl \
            --user "admin:$(pass show majordomo/public/majordomo/jenkins.intr/admin)" \
            --request POST \
            --header 'Content-type: application/x-www-form-urlencoded; charset=UTF-8' \
            -o /dev/null \
            -v \
	    "https://jenkins.corp1.majordomo.ru/job/webservices/job/${job}/job/master/build?delay=0sec"
        sleep 0.5
    done
}

mjru-wp-cron()
{
    nice -n 19 ionice -c2 -n7 find /home/u12345 -type f -name wp-cron.php | xargs -n1 dirname | xargs -n1 -I{} sh -c "echo -n '{} ';grep -rl {} /etc/nginx/sites-available | xargs awk -F'-' '\$1~/proxy_pass/ {print \$2}' | uniq" | awk '{print "* * * * * /opt/"$NF"/bin/php",$(NF-1)"/wp-cron.php"}'
}

# TODO:
# majordomo-ansible-galera()
# {
#     ansible "galera$1.intr" -m copy -a "src=galera$1/mariadb-bin.0029$2 dest=/home/mariadb/mariadb-bin.0029$2" --become && ansible "galera$1.intr" -m file -a "path=/home/mariadb/mariadb-bin.0029$2 owner=mysql group=mysql" --become
# }

mjru-ansible-auth-hosts()
{
    for host in $(ansible all --list-hosts |grep intr); do
        printf "%s%s\n" "$host" "$(ssh -oStrictHostKeyChecking=no "$host" -- uptime)"
    done
}

mjru-web-active-current()
{
    curl -H "PRIVATE-TOKEN: $(pass show majordomo/public/gitlab.intr/tokens/terraform)" -s -k -L \
            'https://gitlab.intr/hms/config-repo/raw/master/rc-staff-prod.yml'
}

mjru-gitlab-version()
{
    curl --header "PRIVATE-TOKEN: $(pass show majordomo/public/gitlab.intr/tokens/terraform)" \
            --silent --insecure --location https://gitlab.intr/api/v4/version
    echo
}

mjru-skopeo-mj()
{
    group="$1"
    image="$2" # ssh-guest-room
    tag="$3"
    tar="$4" || result # docker-archive:/nix/store/dw0qakl4g58n9idsi35vn0m1d92gs0jw-docker-image-ssh-guest-room.tar.gz
    skopeo copy --dest-creds=gradle:"$(pass show majordomo/public/nexus/gradle)" --dest-tls-verify=false "docker-archive:$tar" "docker://docker-registry.intr/$group/$image:$tag"
}

mjru-skopeo-fetch()
{
    image="$1"
    dest="$2"
    skopeo copy --dest-creds=gradle:"$(pass show majordomo/public/nexus/gradle)" --src-tls-verify=false --dest-tls-verify=false "docker://docker-registry.intr/$image" "docker-archive:$dest"
}

# dockerd --insecure-registry https://docker-registry.intr

mjru-nix-ls-store-kvm15()
{
    # $1 example: /nix/store/0i2jd68mp5g6h2sa5k9c85rb80sn8hi9-hello-2.10
    nix ls-store --store http://kvm15.intr:5556/ -lR "$1"
}

mjru-nix-build-kvm15()
{
    pkg="$1"
    nix-build build.nix --option  substituters http://kvm15.intr:5556/ --cores 4 -A nixpkgsUnstable"$pkg" --keep-going --keep-failed "$@"
}

mjru-nix-build-mj()
{
    nix-build \
            --option trusted-public-keys 'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.nixos.intr:6VD7bofl5zZFTEwsIDsUypprsgl7r9I+7OGY4WsubFA=' \
            --substituters 'https://cache.nixos.org/ https://cache.nixos.intr/' \
            --expr "(import <nixpkgs> {overlays = [(import $HOME/src/gitlab.intr/_ci/nixpkgs)];}).$1"
}

mjru-docker-jenkins()
{
    docker -H ssh://dh4-mr.intr exec -it 4649529fa34d "$@"
}

mjru-hms-current-stack()
{
    nginx1="$(curl --silent -u "jenkins:$(pass show majordomo/private/jenkins/jenkins)" -X GET http://nginx1.intr:8080/hms)"
    nginx2="$(curl --silent -u "jenkins:$(pass show majordomo/private/jenkins/jenkins)" -X GET http://nginx2.intr:8080/hms)"
    nginx1_active="$(echo "$nginx1" | jq --raw-output .active)"
    nginx2_active="$(echo "$nginx2" | jq --raw-output .active)"
    if [[ $nginx1_active == $nginx2_active ]]
    then
        echo "Current stack: ${nginx1_active}"
        if [[ $nginx1_active == "hms1" ]]
        then
            target="hms2"
        else
            target="hms1"
        fi
        cat <<EOF

Switch stack example:

    curl -H "Content-Type: application/json" --data '{"available":["hms2","hms1"],"setActive":"$target"}' -u "jenkins:$(pass show majordomo/private/jenkins/jenkins)" -X POST "http://nginx1.intr:8080/hms"
    curl -H "Content-Type: application/json" --data '{"available":["hms2","hms1"],"setActive":"$target"}' -u "jenkins:$(pass show majordomo/private/jenkins/jenkins)" -X POST "http://nginx2.intr:8080/hms"
EOF
    else
        echo "WARNING: Stacks mismatches"
        cat <<EOF
Switch stack example:

    curl -H "Content-Type: application/json" --data '{"available":["hms2","hms1"],"setActive":"hms1"}' -u "jenkins:$(pass show majordomo/private/jenkins/jenkins)" -X POST "http://nginx1.intr:8080/hms"
    curl -H "Content-Type: application/json" --data '{"available":["hms2","hms1"],"setActive":"hms1"}' -u "jenkins:$(pass show majordomo/private/jenkins/jenkins)" -X POST "http://nginx2.intr:8080/hms"
EOF
    fi
}

mjru-hms-auth ()
{
    curl --silent \
         --request POST https://api.majordomo.ru/oauth/token \
         --header 'content-type: application/x-www-form-urlencoded' \
         --header 'x-requested-with: XMLHttpRequest' \
         -d "grant_type=password&username=$IHS_USER&password=$IHS_PASS&client_id=service&client_secret=service_secret" \
        | jq -r '.access_token'
}

mjru-es-xmlrpc()
{
    curl -H 'Content-Type: application/json' \
         -X POST "http://es.intr:9200/nginx-$(date +"%Y.%m.%d")/_search/" \
         --data-binary '{"from":0,"query":{"query_string":{"query":"path.keyword:\"/xmlrpc.php\""}},"size":50,"sort":[{"@timestamp":{"order":"desc"}}]}'
}

mjru-docker-list-intr()
{
    curl -s -X GET -k -u "gradle:$(pass show majordomo/public/nexus/gradle)" https://docker-registry.intr/v2/_catalog \
        | jq -r '.repositories[]'
}

mjru-jenkins-log()
{
    for project in $(curl -s -k "https://admin:$(pass show majordomo/public/jenkins.intr/admin)@jenkins.intr/api/json?pretty=true" | jq -r '.jobs[] | .name'); do
        mkdir -p "$project"
        cd "$project" || return
        for job in $(curl -s -k "https://admin:$(pass show majordomo/public/jenkins.intr/admin)@jenkins.intr/job/$project/api/json" | jq -r '.jobs[] | .url'); do
            job_name="$(echo "$job" | rev | cut -d/ -f 2 | rev)"
            echo "@ $job"
            curl -u "admin:$(pass show majordomo/public/jenkins.intr/admin)" -s -k "$job/job/master/lastBuild/consoleText" > "$job_name.log"
        done
        cd - || return
    done
}

mjru-ansible-swarm-ps-inspect()
{
    # shellcheck disable=SC2016
    ansible swarm -m shell -a 'for c in $(docker ps | grep -v CONTAINER | cut -d " " -f 1 | xargs echo); do docker inspect $c; done' --become
}

mjru-ansible-swarm-network-inspect()
{
    ansible swarm -m shell -a 'docker network ls | cut -d " " -f 1 | grep -v NETWORK | xargs docker network inspect' --become
}

mjru-dns-check()
{
    for dns in 172.16.103.2 172.16.100.3; do
        (echo $dns; time dig +short a "${1:-cerberus.intr}" @$dns) |& xargs echo
    done
}

mjru-jenkins()
{
    JENKINS_URL=https://jenkins.corp1.majordomo.ru                      \
    JENKINS_USER=admin                                                  \
    JENKINS_PASSWORD="$(pass show majordomo/public/jenkins.intr/admin)" \
    jenkins "$@"
}

mjru-vm-vnc()
{
    (
        set -ex

        host="$1"
        vm="$2"

        vncviewer "$host":"$(connect ssh "$host" virsh dumpxml "$vm" | xq -r '.domain.devices.graphics["@port"]')" &
        password="$(ihs vm passwords "$vm" | awk '{ print $NF }')"

        sleep 1
        echo "(window-send-string (format nil \"~a~%\" \"$password\"))" | stumpish -e eval
        sleep 2
        echo "(window-send-string (format nil \"~a~%\" \"root\"))" | stumpish -e eval
        sleep 0.5
        echo "(window-send-string (format nil \"~a~%\" \"$password\"))" | stumpish -e eval
    )
}

mjru-proxy()
{
    ssh -Nf -D 5557 workstation.intr
}

mjru-vlan()
{
    echo "PXE is on VLAN 254"
    echo "chef-server '/usr/sbin/in.tftpd -L -a 192.168.254.1 -s /var/tftpboot'"
}

mjru-python()
{
    echo "Setting HMS_MONGO_PASSWORD environment variable."
    HMS_MONGO_PASSWORD="$(pass show majordomo/public/mongo/hms/admin)" python3
}

mjru-network()
{
    awk '/A\t/ || /A / { print "(\"", $NF, "/32\" . ,(string-to-symbols \"", $1, ".intr\"))" }' \
	~/src/gitlab.intr/net/dns-intr/intr-miran.zone ~/src/gitlab.intr/net/dns-intr/intr-datahouse.zone \
	| sed 's/" /"/g; s@ /@/@g; s/ ")/")/g; s/"\./" ./; s/ \.intr/.intr/; s/\"\;/"/'

    awk '/A\t/ || /A / { print "(\"inet" , $NF, "/24\" . ,(string-to-symbols \"", $1, ".intr\"))" }' \
	~/src/gitlab.intr/net/dns-intr/intr-miran.zone ~/src/gitlab.intr/net/dns-intr/intr-datahouse.zone \
	| sed 's/" /"/g; s@ /@/@g; s/ ")/")/g; s/"\./" ./; s/ \.intr/.intr/; s/\"\;/"/'

    awk '/A\t/ || /A / { print "(\"", $NF, "\" . ,(string-to-symbols \"", $1, ".intr\"))" }' \
	~/src/gitlab.intr/net/dns-intr/intr-miran.zone ~/src/gitlab.intr/net/dns-intr/intr-datahouse.zone \
	 | sed 's/" /"/g; s@ /@/@g; s/ ")/")/g; s/ "\./" ./; s/ \.intr/.intr/; s/\"\;/"/'
}

vault1()
{
    VAULT_TOKEN="$(pass show majordomo/public/vault/root)" VAULT_ADDR=http://dh1-mr.intr:8210 "${HOME}/.nix-profile/bin/vault" "$@"
}

vault2()
{
    VAULT_TOKEN="$(pass show majordomo/public/vault/root)" VAULT_ADDR=http://dh2-mr.intr:8220 "${HOME}/.nix-profile/bin/vault" "$@"
}

vault3()
{
    VAULT_TOKEN="$(pass show majordomo/public/vault/root)" VAULT_ADDR=http://dh3-mr.intr:8230 "${HOME}/.nix-profile/bin/vault" "$@"
}

vault4()
{
    VAULT_TOKEN="$(pass show majordomo/public/vault-unseal/root)" VAULT_ADDR=http://dh4-mr.intr:8240 "${HOME}/.nix-profile/bin/vault" "$@"
}

vault-ci()
{
    VAULT_TOKEN="$(pass show majordomo/public/vault-dev/root)" VAULT_ADDR=http://ci.intr:8210 "${HOME}/.nix-profile/bin/vault" "$@"
}

mjru-gitlab()
{
    GITLAB_HOST=https://gitlab.intr \
               GITLAB_TOKEN="$(pass show majordomo/private/gitlab.intr/tokens/pyhalov)" \
               command glab "$@"
}

if [ -f "${HOME}/.nix-profile/bin/glab" ]
then
    if [[ $(type -t compopt) = "builtin" ]]; then
        complete -o default -F __start_glab mjru-gitlab
    else
        complete -o default -o nospace -F __start_glab mjru-gitlab
    fi
fi

mjru-curl()
{
    curl --header "Authorization: Bearer $(mjru-auth)" \
         --header 'Content-Type: application/json' \
         "$@"
}

mongo()
{
    case "$1" in
        cluster1)
            command mongo "mongodb://admin:$(pass show majordomo/public/mongo/ci.intr/admin)@mongodb.hms.development.corp1.majordomo.ru:27017/admin" \
                    "${@:2}"
            ;;
        ci)
            command mongo "mongodb://admin:$(pass show majordomo/public/mongo/ci.intr/admin)@ci.intr:27017/admin" \
                    "${@:2}"
            ;;
        production)
            command mongo "mongodb://admin:$(pass show majordomo/public/mongo/ci.intr/admin)@hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/admin?replicaSet=hms-rs0" \
                    "${@:2}"
            ;;
        *)
            command mongo "$@"
    esac
}

mongodump()
{
    case "$1" in
        cluster1)
            command mongodump \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://mongodb.hms.development.corp1.majordomo.ru:27017/" \
                    "${@:2}"
            ;;
        ci)
            command mongodump \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://ci.intr:27017/" \
                    "${@:2}"
            ;;
        production)
            command mongodump \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/?replicaSet=hms-rs0" \
                    "${@:2}"
            ;;
        *)
            command mongodump "$@"
    esac
}

mongorestore()
{
    case "$1" in
        cluster1)
            command mongorestore \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://mongodb.hms.development.corp1.majordomo.ru:27017/" \
                    "${@:2}"
            ;;
        ci)
            command mongorestore \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://ci.intr:27017/" \
                    "${@:2}"
            ;;
        alerta)
            command mongorestore \
                    --authenticationDatabase=admin \
                    --username=root \
                    --password="$(pass show majordomo/public/alerta/mongodb/root)" \
                    --uri="mongodb://localhost:27017/" \
                    "${@:2}"
            ;;
        production)
            command mongorestore \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/?replicaSet=hms-rs0" \
                    "${@:2}"
            ;;
        *)
            command mongorestore "$@"
    esac
}

mongoexport()
{
    case "$1" in
        cluster1)
            command mongoexport \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://mongodb.hms.development.corp1.majordomo.ru:27017/" \
                    "${@:2}"
            ;;
        ci)
            command mongoexport \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://ci.intr:27017/" \
                    "${@:2}"
            ;;
        production)
            command mongoexport --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/?replicaSet=hms-rs0" \
                    "${@:2}"
            ;;
        *)
            command mongoexport "$@"
    esac
}

mongoimport()
{
    case "$1" in
        cluster1)
            command mongoimport \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://mongodb.hms.development.corp1.majordomo.ru:27017/" \
                    "${@:2}"
            ;;
        ci)
            command mongoimport \
                    --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://ci.intr:27017/" \
                    "${@:2}"
            ;;
        production)
            command mongoimport --authenticationDatabase=admin \
                    --username=admin \
                    --password="$(pass show majordomo/public/mongo/ci.intr/admin)" \
                    --uri="mongodb://hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/?replicaSet=hms-rs0" \
                    "${@:2}"
            ;;
        *)
            command mongoimport "$@"
    esac
}
