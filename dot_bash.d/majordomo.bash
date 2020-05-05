majordomo-backup()
{
    case "$1" in
        list)
            if [ -z "$2" ]; then
                echo "provide unix_account_name"
                echo "example: backup_list u168138"
                (exit 1)
            else curl -s bareos.intr/_snapshot/slice/$(echo -n $2 | sha1sum  |  cut -c -2)/$2 | jq -r '.[] | [.dataUri.rsync, .time] | @tsv'
            fi
            ;;        
        mount)
            if [ -z "$2" ]; then
                echo "provide unix_account_name"
                echo "example: backup_mount u168138"
                (exit 1)
            else curl -s -XPOST  "bareos.intr/_mount/slice/$(echo -n $2 | sha1sum  |  cut -c -2)/$2?wait=True&timeout=600" | jq -r
            fi
            ;;
        umount)
            if [ -z "$2" ]; then
                echo "provide unix_account_name"
                echo "example: backup_umount u168138"
                (exit 1)
            else curl -s -XDELETE  "bareos.intr/_mount/slice/$(echo -n $2 | sha1sum  |  cut -c -2)/$2" | jq -r
            fi
            ;;
    esac
}

majordomo-docker()
{
    case "$1" in
        registry)
            docker-ls repositories --registry https://docker-registry.intr/ --allow-insecure "${@:2}"
            ;;
        tags)
            docker-ls tags --registry https://docker-registry.intr/ --allow-insecure "${@:2}"
            ;;
        tag)
            docker-ls tag --registry https://docker-registry.intr/ --allow-insecure "${@:2}"
            ;;
    esac
}

majordomo-influx()
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

majordomo-sshuttle()
{
    sshuttle -r majordomo 10.0.0.0/8 172.16.0.0/16
}

majordomo-vnc()
{
    vncviewer "kvm$1":$(( $2 + 5900 ))
}

majordomo-galera-df-home()
{
    for n in 1 2 3; do
        echo -e "\n@ galera$n.intr"
        ssh "galera$n.intr" -- df -h /home
    done
}

router4.intr()
{
    sshpass -p"$(pass show majordomo/router4/root)" ssh router4.intr
}

router.majordomo.ru()
{
    ssh -t work
    # sshpass -p$(pass show majordomo/router.majordomo.ru) ssh -vvv -oKexAlgorithms=+diffie-hellman-group1-sha1 -oHostKeyAlgorithms=+ssh-dss -p 1022 -l root -i ~/.ssh/eng_key_rsa router.majordomo.ru
}

br1-mr14.intr-ftp-list()
{
    curl "ftp://netcfg:$(pass show majordomo/172.16.103.111/netcfg)@172.16.103.111/junos/"
}

br1-mr14.intr-ftp()
{
    # Example “config”: br1-mr14.intr_juniper.conf.gz_20190702_170649
    config="$1"
    wget -O- "ftp://netcfg:$(pass show majordomo/172.16.103.111/netcfg)@172.16.103.111/junos/$config" | zcat
}

majordomo-juneos-config()
{
    sshpass -p$(pass show majordomo/ssh/router) ssh -l root "$1" -- 'cli -c "show config | display xml"'
}

majordomo-br1-mr14.intr-xq-br()
{
    ssh -l root br1-mr14.intr -- 'cli -c "show interfaces | display xml"' \
        | xq -y '."rpc-reply"."interface-information"."physical-interface"[] | ."logical-interface" | select(. != null)'
}

majordomo-backup-mount()
{
    sudo -u majordomo-ssh-tunnel restic -r /srv/backup/majordomo mount /mnt/backup
}

majordomo-nix-repl()
{
    echo "overlay = lib.listToAttrs (map (drv: lib.nameValuePair drv.name drv) (import ./build.nix))"
}

majordomo-nix-fix()
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
        sed -i 's|(builtins.fetchGit { url = "git@gitlab.intr:_ci/nixpkgs.git"; inherit ref; })|/home/oleg/majordomo/_ci/nixpkgs|g' "$file"
    done
}

majordomo-jenkins-build-php()
{
    branch="$1"
    for job in apache2-php52 apache2-php53 apache2-php54 apache2-php55 apache2-php56 apache2-php70 apache2-php71 apache2-php72 apache2-php73 apache2-php74; do
        echo -e "\n@ $job"
        curl -u "pyhalov:$(pass show jenkins.intr/pyhalov)" -s -k \
"https://jenkins.intr/job/webservices/job/$job/job/$branch/build?delay=0sec" \
-H 'Content-type: application/x-www-form-urlencoded; charset=UTF-8' --data-urlencode json='{"parameter": [{"name":"DEPLOY", "value":"true"}]}'
        sleep 0.5
    done
}

majordomo-wp-cron()
{
    dir="$1"
    nice -n 19 ionice -c2 -n7 find /home/u12345 -type f -name wp-cron.php | xargs -n1 dirname | xargs -n1 -I{} sh -c "echo -n '{} ';grep -rl {} /etc/nginx/sites-available | xargs awk -F'-' '\$1~/proxy_pass/ {print \$2}' | uniq" | awk '{print "* * * * * /opt/"$NF"/bin/php",$(NF-1)"/wp-cron.php"}'
}

# TODO:
# majordomo-ansible-galera()
# {
#     ansible "galera$1.intr" -m copy -a "src=galera$1/mariadb-bin.0029$2 dest=/home/mariadb/mariadb-bin.0029$2" --become && ansible "galera$1.intr" -m file -a "path=/home/mariadb/mariadb-bin.0029$2 owner=mysql group=mysql" --become
# }

majordomo-ansible-auth-hosts()
{
    for host in $(ansible all --list-hosts |grep intr); do
        printf "%s%s\n" "$host" "$(ssh -oStrictHostKeyChecking=no $host -- uptime)"
    done
}

majordomo-web-active-current()
{
    curl -H "PRIVATE-TOKEN: $(pass show majordomo/gitlab.intr/tokens/terraform)" -s -k -L \
            'https://gitlab.intr/hms/config-repo/raw/master/rc-staff-prod.yml'
}

majordomo-gitlab-version()
{
    curl --header "PRIVATE-TOKEN: $(pass show majordomo/gitlab.intr/tokens/terraform)" \
            --silent --insecure --location https://gitlab.intr/api/v4/version
    echo
}

majordomo-skopeo-mj()
{
    group="$1"
    image="$2" # ssh-guest-room
    tag = "$3"
    tar="$4" || result # docker-archive:/nix/store/dw0qakl4g58n9idsi35vn0m1d92gs0jw-docker-image-ssh-guest-room.tar.gz
    skopeo copy --dest-creds=gradle:"$(pass show majordomo/nexus/gradle)" --dest-tls-verify=false "docker-archive:$tar" "docker://docker-registry.intr/$group/$image:$tag"
}

majordomo-skopeo-fetch()
{
    image="$1"
    dest="$2"
    skopeo copy --dest-creds=gradle:"$(pass show majordomo/nexus/gradle)" --src-tls-verify=false --dest-tls-verify=false "docker://docker-registry.intr/$image" "docker-archive:$dest"
}

# dockerd --insecure-registry https://docker-registry.intr

majordomo-nix-ls-store-kvm15()
{
    # $1 example: /nix/store/0i2jd68mp5g6h2sa5k9c85rb80sn8hi9-hello-2.10
    nix ls-store --store http://kvm15.intr:5556/ -lR "$1"
}

majordomo-nix-build-kvm15()
{
    pkg="$1"
    nix-build build.nix --option  substituters http://kvm15.intr:5556/ --cores 4 -A nixpkgsUnstable$pkg --keep-going --keep-failed $@
}

majordomo-nix-build-mj()
{
    nix-build \
            --option trusted-public-keys 'cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.nixos.intr:6VD7bofl5zZFTEwsIDsUypprsgl7r9I+7OGY4WsubFA=' \
            --substituters 'https://cache.nixos.org/ https://cache.nixos.intr/' \
            --expr "(import <nixpkgs> {overlays = [(import $HOME/majordomo/_ci/nixpkgs)];}).$1"
}

majordomo-docker-jenkins()
{
    docker -H ssh://dh4-mr.intr exec -it 4649529fa34d $@
}

majordomo-hms-current-stack()
{
    curl -u "jenkins:$(pass show majordomo/jenkins/jenkins)" -X GET http://nginx1.intr:8080/hms
}

majordomo-hms-auth ()
{
    curl --silent \
         --request POST https://api.majordomo.ru/oauth/token \
         --header 'content-type: application/x-www-form-urlencoded' \
         --header 'x-requested-with: XMLHttpRequest' \
         -d "grant_type=password&username=$IHS_USER&password=$IHS_PASS&client_id=service&client_secret=service_secret" \
        | jq -r '.access_token'
}

majordomo-es-xmlrpc()
{
    curl -H 'Content-Type: application/json' \
         -X POST "http://es.intr:9200/nginx-$(date +"%Y.%m.%d")/_search/" \
         --data-binary '{"from":0,"query":{"query_string":{"query":"path.keyword:\"/xmlrpc.php\""}},"size":50,"sort":[{"@timestamp":{"order":"desc"}}]}'
}

majordomo-docker-list-intr()
{
    curl -s -X GET -k -u "gradle:$(pass show majordomo/nexus/gradle)" https://docker-registry.intr/v2/_catalog \
        | jq -r '.repositories[]'
}

majordomo-jenkins-log()
{
    for project in $(curl -s -k 'https://admin:$(pass show jenkins.intr/admin)@jenkins.intr/api/json?pretty=true' | jq -r '.jobs[] | .name'); do
        mkdir -p "$project"
        cd "$project"
        for job in $(curl -s -k "https://admin:$(pass show jenkins.intr/admin)@jenkins.intr/job/$project/api/json" | jq -r '.jobs[] | .url'); do
            job_name="$(echo $job | rev | cut -d/ -f 2 | rev)"
            echo "@ $job"
            curl -u 'admin:$(pass show jenkins.intr/admin)' -s -k "$job/job/master/lastBuild/consoleText" > "$job_name.log"
        done
        cd -
    done
}

majordomo-ansible-swarm-ps-inspect()
{
    ansible swarm -m shell -a 'for c in $(docker ps | grep -v CONTAINER | cut -d " " -f 1 | xargs echo); do docker inspect $c; done' --become
}

majordomo-ansible-swarm-network-inspect()
{
    ansible swarm -m shell -a 'docker network ls | cut -d " " -f 1 | grep -v NETWORK | xargs docker network inspect' --become
}

majordomo-add-hosts-mikrotik()
{
    # Add hosts from Majordomo to MikroTik.
    for host in $@; do
        ssh mikrotik -- /ip dns static add address="$(ssh majordomo -- dig +short a $host.intr 2>/dev/null)" name="$host.intr";
    done
}

majordomo-jenkins-log()
{
    for project in $(curl -s -k "https://admin:$(pass show jenkins.intr/admin)@jenkins.intr/api/json?pretty=true" | jq -r '.jobs[] | .name'); do
        mkdir -p "$project"
        cd "$project"
        for job in $(curl -s -k "https://admin:$(pass show jenkins.intr/admin)@jenkins.intr/job/$project/api/json" | jq -r '.jobs[] | .url'); do
            job_name="$(echo $job | rev | cut -d/ -f 2 | rev)"
            echo "@ $job"
            curl -u "admin:$(pass show jenkins.intr/admin)" -s -k "$job/job/master/lastBuild/consoleText" > "$job_name.log"
        done
        cd -
    done
}
