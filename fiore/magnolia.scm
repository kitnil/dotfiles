;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017, 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu) (sysadmin services) (sysadmin sensors))
(use-package-modules bash bootloaders linux monitoring networking php)
(use-service-modules admin certbot databases dns networking rsync
                     shepherd spice ssh virtualization web cgit
                     version-control monitoring)

(define %source-dir (dirname (current-filename)))


;;;
;;; Networking
;;;

(define start-networking
  #~(let ((ip
           (lambda (str)
             (zero? (system (string-join `(,#$(file-append iproute "/sbin/ip")
                                           ,str) " "))))))
      (format #t "Install interface rules.~%")
      (and (ip "tuntap add tap0 mode tap")
           (ip "tuntap add tap1 mode tap")
           (ip "address add 192.168.55.1/24 dev tap0")
           (ip "address add 192.168.60.1/24 dev tap1")
           (ip "link set tap0 up")
           (ip "link set tap1 up"))))

(define custom-networking-service
  (simple-service 'custom-networking shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(custom-networking))
                    (requirement '())
                    (start #~(lambda _
                               #$start-networking))
                    (respawn? #f)))))


;;;
;;; DNS
;;;

(define-zone-entries tld.zone
  ;; Name TTL Class Type Data
  ("@" "" "IN" "A" "127.0.0.1")
  ("@" "" "IN" "NS" "ns")
  ("ns" "" "IN" "A" "127.0.0.1")
  ("cuirass.tld." "" "IN" "A"  "192.168.105.120")
  ("www.cuirass.tld." "" "IN" "A"  "192.168.105.120")
  ("cups.tld." "" "IN" "A" "192.168.105.120")
  ("www.cups.tld." "" "IN" "A" "192.168.105.120")
  ("git.tld." "" "IN" "A" "192.168.105.120")
  ("www.git.tld." "" "IN" "A" "192.168.105.120")
  ("guix.tld." "" "IN" "A" "192.168.105.120")
  ("www.guix.tld." "" "IN" "A" "192.168.105.120")
  ("input.tld." "" "IN" "A" "192.168.105.120")
  ("www.input.tld." "" "IN" "A" "192.168.105.120")
  ("torrent.tld." "" "IN" "A" "192.168.105.120")
  ("www.torrent.tld." "" "IN" "A" "192.168.105.120")
  ("www.tld." "" "IN" "A" "192.168.105.120")
  ("zabbix.tld." "" "IN" "A" "192.168.105.120")
  ("www.zabbix.tld." "" "IN" "A" "192.168.105.120")
  ("r1.tld." "" "IN" "A" "192.168.100.1")
  ("www.r1.tld." "" "IN" "A" "192.168.100.1")
  ("r2.tld." "" "IN" "A" "192.168.105.1")
  ("www.r2.tld." "" "IN" "A" "192.168.105.1"))

(define master-zone
  (knot-zone-configuration
   (domain "tld")
   (zone (zone-file
          (origin "tld")
          (entries tld.zone)))))


;;;
;;; NGINX
;;;

;; (define local-esxi-publish-nginx-service
;;   (simple-service 'guix-publish-nginx nginx-service-type
;;    (list (nginx-server-configuration
;;           (server-name '("esxi.local"))
;;           (listen '("443"))
;;           (locations (list (nginx-location-configuration
;;                             (uri "/")
;;                             (body (list "resolver 192.168.105.120;"
;;                                         (string-append "set $target localhost:" (number->string 17443) ";")
;;                                         "proxy_pass https://$target;"
;;                                         (format #f "proxy_set_header Host ~a;" "192.168.125.22")
;;                                         "proxy_set_header X-Real-IP $remote_addr;"
;;                                         "proxy_set_header X-Forwarded-for $remote_addr;"
;;                                         "proxy_connect_timeout 300;")))))
;;           (ssl-certificate #f)
;;           (ssl-certificate-key #f)))))

(define %nginx-server-blocks
  (list (nginx-server-configuration
         (server-name '("www.tld"))
         (listen '("80"))
         (root "/srv/share"))
        (proxy "cups.tld" 631)
        (proxy "torrent.tld" 9091)
        (proxy "jenkins.wugi.info" 30080 #:ssl? #t)
        (proxy "grafana.wugi.info" 3080 #:ssl? #t)
        (proxy "gitlab.wugi.info" 65080  #:ssl? #t)
        (proxy "anongit.duckdns.org" 65080  #:ssl? #t)
        (proxy "cuirass.tld" 19080)
        (proxy "input.tld" 19080)
        (proxy "prometheus.wugi.info" 65090 #:ssl? #t)
        (proxy "alerta.intr" 16180)
        (proxy "web.alerta.intr" 16480)
        (proxy "zabbix.intr" 15081)
        (proxy "cerberus.intr" 15080)
        (proxy "grafana.intr" 16080)
        (proxy "guix.duckdns.org" 3000 #:ssl? #t)
        (nginx-server-configuration
         (server-name '("user.tld"))
         (listen '("80"))
         (root "/home/natsu/public_html"))))


;;;
;;; Operating system.
;;;

(let ((ip-address "192.168.105.120"))
  (operating-system
    (host-name "magnolia")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")))

    (file-systems (cons* (file-system
                           (device (file-system-label "magnolia-root"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device "/dev/sda1")
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device (file-system-label "magnolia-data"))
                           (mount-point "/srv")
                           (type "ext4"))
                         (file-system
                           (device "tmpfs")
                           (mount-point "/tmp")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(no-dev))
                           (options "mode=1777,size=50%"))
                         %base-file-systems))

    (swap-devices '("/dev/sda3"))

    (groups (cons* (user-group (name "adbusers"))
                   (user-group (name "guix-offload"))
                   (user-group (name "telegraf") (system? #t))
                   (user-group (name "git") (id 30003))
                   (user-group (name "jenkins") (id 30004))
                   (user-group (name "influxdb") (id 30005))
                   (user-group (name "grafana") (id 30006))
                   (user-group (name "docker") (id 30007))
                   (user-group (name "jenkinsbuild"))
                   (user-group (name "alerta"))
                   %base-groups))

    (users (cons* (user-account
                   (name "natsu")
                   (uid 1000)
                   (comment "Oleg Pykhalov")
                   (group "users")
                   (supplementary-groups '("wheel"
                                           "audio" "video"
                                           "lpadmin" "lp"
                                           "adbusers" "libvirt"
                                           "kvm"
                                           "docker"))
                   (home-directory "/home/natsu"))
                  (user-account
                   (name "guix-offload")
                   (uid 1982)
                   (group "guix-offload")
                   (home-directory "/home/guix-offload"))
                  (user-account
                   (name "ssh-tunnel")
                   (group "users")
                   (comment "SSH forwarding privilege separation user")
                   (home-directory "/home/ssh-tunnel"))
                  (user-account
                   (name "majordomo-ssh-tunnel")
                   (group "users")
                   (comment "SSH forwarding privilege separation user")
                   (home-directory "/home/majordomo-ssh-tunnel"))
                  (user-account
                   (name "majordomo-backup")
                   (group "users")
                   (comment "SSH for backup privilege separation user")
                   (home-directory "/home/majordomo-backup"))
                  (user-account
                   (name "tail-ssh-tunnel")
                   (group "users")
                   (comment "SSH forwarding privilege separation user")
                   (home-directory "/home/tail-ssh-tunnel"))
                  (user-account
                   (name "tail-backup")
                   (group "users")
                   (comment "SSH for backup privilege separation user")
                   (home-directory "/home/tail-backup"))
                  (user-account
                   (name "anonymous")
                   (group "users")
                   (comment "Anonymous user")
                   (home-directory "/home/anonymous"))
                  (user-account
                   (name "git")
                   (group "git")
                   (uid 30017)
                   (comment "SSH privilege separation user")
                   (home-directory "/home/git"))
                  (user-account
                   (name "jenkins")
                   (group "jenkins")
                   (uid 30018)
                   (comment "Jenkins privilege separation user")
                   (home-directory "/home/jenkins"))
                  (user-account
                   (name "jenkinsbuilder01")
                   (group "jenkinsbuild")
                   (comment "Guix Build User  1")
                   (home-directory "/home/jenkinsbuild"))
                  (user-account
                   (name "influxdb")
                   (group "influxdb")
                   (uid 30019)
                   (comment "influxdb privilege separation user")
                   (home-directory "/home/influxdb"))
                  (user-account
                   (name "grafana")
                   (group "grafana")
                   (uid 30020)
                   (comment "grafana privilege separation user")
                   (home-directory "/home/grafana"))
                  (user-account
                   (name "alerta")
                   (group "alerta")
                   (comment "alerta privilege separation user")
                   (home-directory "/home/alerta"))
                  %base-user-accounts))

    ;; Create a /etc/hosts file with aliases for "localhost"
    ;; and "mymachine", as well as for Facebook servers.
    (hosts-file
     (plain-file
      "hosts"
      (string-append
       (local-host-aliases host-name)
       "\n"
       (prefix-local-host-aliases
        #:prefixes '("cgit" "anongit" "guix" "alerta" "weblog")
        #:host-name "duckdns"
        #:domain ".org"
        #:ip-addresses (list ip-address))
       "\n"
       (prefix-local-host-aliases
        #:prefixes '("cuirass"
                     "www.cuirass"
                     "cups"
                     "www.cups"
                     "git"
                     "www.git"
                     "guix"
                     "www.guix"
                     "input"
                     "www.input"
                     "torrent"
                     "www.torrent"
                     "www"
                     "zabbix"
                     "www.zabbix"
                     "jenkins"
                     "www.jenkins")
        #:host-name "tld"
        #:domain ""
        #:ip-addresses (list ip-address))
       "\n"
       (prefix-local-host-aliases
        #:prefixes '("alerta" "cerberus" "grafana"
                     "rpc-mj" "web.alerta" "zabbix")
        #:host-name ""
        #:domain "intr"
        #:ip-addresses (list ip-address))
       "\n\n"
       (serialize-hosts '(("192.168.100.1" . "r1.tld")
                          ("192.168.100.1" . "www.r1.tld")
                          ("192.168.105.1" . "r2.tld")
                          ("192.168.105.1" . "www.r2.tld")
                          ("192.168.105.120" . "hms-billing.majordomo.ru")
                          ("192.168.105.120" . "anongit.duckdns.org")
                          ("192.168.105.120" . "pipeline.duckdns.org")
                          ))
       "\n\n"
       (prefix-local-host-aliases #:prefixes '("gitlab" "grafana" "jenkins" "prometheus" "zabbix")
                                  #:host-name "wugi"
                                  #:domain ".info"
                                  #:ip-addresses (list ip-address))
       "\n\n"
       %facebook-host-aliases)))

    (packages (custom-packages (string-append %source-dir
                                              "/manifests/fiore.scm")))

    (services (cons*
               custom-networking-service
               (static-networking-service "enp6s0" ip-address
                                          #:netmask "255.255.255.0"
                                          #:gateway "192.168.105.1"
                                          ;; See <http://www.freenom.world>.
                                          #:name-servers '("80.80.80.80"
                                                           "80.80.81.81")
                                          ;; TODO: Fix Knot resolve
                                          ;; '("192.168.105.120")
                                          )

               (service ddclient-service-type)

               (service rottlog-service-type
                        (rottlog-configuration
                         (inherit (rottlog-configuration))
                         (rotations (cons (log-rotation
                                           (files '("/var/log/nginx/access.log"
                                                    "/var/log/nginx/error.log"))
                                           (frequency 'daily))
                                          (map (lambda (rotation)
                                                 (log-rotation
                                                  (inherit rotation)
                                                  (frequency 'daily)))
                                               %default-rotations)))))

               ;; TODO: Publish new fields to upstream.
               #;(service knot-service-type
                        (knot-configuration
                         (zones (list master-zone))
                         (listen-v6 #f)
                         (extra-options "
remote:
 - id: hidden
   address: 80.80.80.80

mod-dnsproxy:
  - id: default
    remote: hidden

template:
  - id: default
    global-module: mod-dnsproxy/default
")))

               (service libvirt-service-type
                        (libvirt-configuration
                         (unix-sock-group "libvirt")
                         (tls-port "16555")))

               (service virtlog-service-type)

               (service guix-publish-service-type
                        (guix-publish-configuration
                         (host "0.0.0.0") (port 3000)))

               (service rsync-service-type)

               (service fcgiwrap-service-type)

               (service zabbix-server-service-type
                        (zabbix-server-configuration
                         (include-files '("/etc/zabbix/zabbix-server.secret"))
                         (extra-options "
AlertScriptsPath=/etc/zabbix/alertscripts
ExternalScripts=/etc/zabbix/externalscripts
FpingLocation=/run/setuid-programs/fping
")))

               (service zabbix-agent-service-type
                        (zabbix-agent-configuration
                         (extra-options "\
UserParameter=guix.channel.list[*],/etc/zabbix/externalscripts/zabbix_guix --profile=$1 --remote=$2 --available
UserParameter=guix.channel.diff[*],/etc/zabbix/externalscripts/zabbix_guix --profile=$1 --remote=$2 --diff=$3
UserParameter=cpu.sensors,/etc/zabbix/externalscripts/zabbix_sensors
")))

               (service zabbix-front-end-service-type
                        (zabbix-front-end-configuration
                         (db-secret-file "/etc/zabbix/zabbix.secret")
                         (nginx
                          (list
                           (nginx-server-configuration
                            (inherit %zabbix-front-end-configuration-nginx)
                            (server-name '("zabbix.wugi.info" "alerta.duckdns.org" "zabbix.tld"))
                            (locations
                             (cons* (nginx-location-configuration
                                     (inherit php-location)
                                     (uri "/describe/natsu")
                                     (body (append '("alias /var/www/php;")
                                                   (nginx-location-configuration-body (nginx-php-location)))))
                                    ;; For use by Certbot.
                                    (nginx-location-configuration
                                     (uri "/.well-known")
                                     (body '("root /var/www;")))
                                    (nginx-server-configuration-locations %zabbix-front-end-configuration-nginx)))
                            (listen '("443 ssl"))
                            (ssl-certificate (letsencrypt-certificate "zabbix.wugi.info"))
                            (ssl-certificate-key (letsencrypt-key "zabbix.wugi.info")))))))

               (postgresql-service)

               (service php-fpm-service-type
                        (php-fpm-configuration
                         (timezone "Europe/Moscow")))

               (service nginx-service-type
                        (nginx-configuration
                         (server-blocks %nginx-server-blocks)))

               (spice-vdagent-service)

               (service cgit-service-type
                        (cgit-configuration
                         (branch-sort "age")
                         (enable-commit-graph? #t)
                         (enable-follow-links? #t)
                         (enable-index-links? #t)
                         (enable-log-filecount? #t)
                         (enable-log-linecount? #t)
                         (enable-remote-branches? #t)
                         (enable-subject-links? #t)
                         (remove-suffix? #t)
                         (enable-index-owner? #f)
                         (root-title (string-join (list "Cgit on" host-name)))
                         (snapshots (list "tar.gz"))
                         (clone-prefix (list ;; "git://magnolia.local/~natsu"
                                        "https://cgit.duckdns.org/git"))
                         (nginx (list (nginx-server-configuration
                                       (inherit %cgit-configuration-nginx)
                                       (server-name '("cgit.duckdns.org" "git.tld"))
                                       (locations
                                        (append (nginx-server-configuration-locations %cgit-configuration-nginx)
                                                (list (git-http-nginx-location-configuration
                                                       (git-http-configuration
                                                        (export-all? #t)))
                                                      ;; For use by Certbot.
                                                      (nginx-location-configuration
                                                       (uri "/.well-known")
                                                       (body '("root /var/www;"))))))
                                       (listen '("80" "443 ssl"))
                                       (ssl-certificate (letsencrypt-certificate "cgit.duckdns.org"))
                                       (ssl-certificate-key (letsencrypt-key "cgit.duckdns.org")))))))

               (service certbot-service-type
                        (certbot-configuration
                         (email "go.wigust@gmail.com")
                         (certificates
                          `(,@(map (lambda (host)
                                     (certificate-configuration
                                      (domains (list host))
                                      (deploy-hook %nginx-deploy-hook)))
                                   (list "cgit.duckdns.org"
                                         "guix.duckdns.org"
                                         "alerta.duckdns.org"
                                         "anongit.duckdns.org"
                                         "pipeline.duckdns.org"
                                         ;; TODO: "wugi.info"
                                         "zabbix.wugi.info"
                                         "grafana.wugi.info"
                                         "jenkins.wugi.info"
                                         "gitlab.wugi.info"
                                         "prometheus.wugi.info"))))))

               (extra-special-file "/bin/sh"
                                   (file-append bash "/bin/sh"))
               (extra-special-file "/usr/bin/env"
                                   (file-append coreutils "/bin/env"))
               (extra-special-file "/etc/zabbix/externalscripts/zabbix_sensors"
                                   zabbix-sensors)

               (custom-desktop-services
                #:tor-config-file (string-append %source-dir "/torrc"))))

    (setuid-programs (cons (file-append ubridge "/bin/ubridge")
                           %setuid-custom-programs))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

;;; magnolia.scm ends here
