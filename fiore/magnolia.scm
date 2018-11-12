;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu) (sysadmin services)
             (packages php) (services monitoring))
(use-package-modules bash bootloaders linux monitoring networking)
(use-service-modules certbot databases dns networking rsync shepherd
                     spice ssh virtualization web cgit version-control)

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
        (nginx-server-configuration
         (server-name '("cups.tld" "www.cups.tld"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body '("proxy_pass http://localhost:631;"))))))
        (nginx-server-configuration
         (server-name '("alerta.duckdns.org" "zabbix.tld"))
         (root (file-append zabbix-server "/share/zabbix/php"))
         (index '("index.php"))
         (locations
          (list (let ((php-location (nginx-php-location)))
                  (nginx-location-configuration
                   (inherit php-location)
                   (body (append (nginx-location-configuration-body php-location)
                                 (list "fastcgi_param PHP_VALUE \"post_max_size = 16M\nmax_execution_time = 300\";")))))))
         (listen '("80" "443 ssl"))
         (ssl-certificate (letsencrypt-certificate "alerta.duckdns.org"))
         (ssl-certificate-key (letsencrypt-key "alerta.duckdns.org")))
        (nginx-server-configuration
         (server-name '("torrent.tld" "www.torrent.tld"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body '("proxy_pass http://localhost:9091;"))))))
        (nginx-server-configuration
         (server-name '("cuirass.tld" "www.cuirass.tld"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "cuirass.tld" 19080))))))
        (nginx-server-configuration
         (server-name '("input.tld" "www.input.tld"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "input.tld" 19080))))))
        (nginx-server-configuration
         (server-name '("hms-billing.majordomo.ru"))
         (listen '("443"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "hms-billing.majordomo.ru" 16280 "https"))))))
        (nginx-server-configuration
         (server-name '("rpc-mj.intr"))
         (listen '("443"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "rpc-mj.intr" 16280 "https"))))))
        (nginx-server-configuration
         (server-name '("alerta.intr"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "alerta.intr" 16180))))))
        (nginx-server-configuration
         (server-name '("web.alerta.intr"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "web.alerta.intr" 16480))))))
        (nginx-server-configuration
         (server-name '("zabbix.intr"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "zabbix.intr" 15081))))))
        (nginx-server-configuration
         (server-name '("cerberus.intr"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "cerberus.intr" 15080))))))
        (nginx-server-configuration
         (server-name '("grafana.intr"))
         (listen '("80"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body (proxy "grafana.intr" 16080))))))
        (nginx-server-configuration
         (server-name '("guix.duckdns.org" "guix.tld" "www.guix.tld"))
         (listen '("80" "443 ssl"))
         (ssl-certificate (letsencrypt-certificate "guix.duckdns.org"))
         (ssl-certificate-key (letsencrypt-key "guix.duckdns.org"))
         (locations
          (list (nginx-location-configuration
                 (uri "/")
                 (body '("proxy_pass http://localhost:3000;"))))))
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
                                           "kvm"))
                   (home-directory "/home/natsu"))
                  (user-account
                   (name "guix-offload")
                   (uid 1982)
                   (group "guix-offload")
                   (home-directory "/home/guix-offload"))
                  (user-account
                   (name "telegraf")
                   (system? #t)
                   (group "telegraf")
                   (comment "telegraf privilege separation user")
                   (home-directory "/var/run/telegraf"))
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
        #:prefixes '("alerta" "cerberus" "grafana"
                     "rpc-mj" "web.alerta" "zabbix")
        #:host-name ""
        #:domain "intr"
        #:ip-addresses (list ip-address))
       "\n\n"
       (serialize-hosts '(("192.168.105.120" . "hms-billing.majordomo.ru")))
       "\n\n" %facebook-host-aliases)))

    (packages (custom-packages (string-append %source-dir
                                              "/manifests/fiore.scm")))

    (services (cons*
               custom-networking-service
               (static-networking-service "enp6s0" ip-address
                                          #:netmask "255.255.255.0"
                                          #:gateway "192.168.105.1"
                                          ;; See <http://www.freenom.world>.
                                          #:name-servers '("192.168.105.120"))

               (service ddclient-service-type)

               (service knot-service-type
                        (knot-configuration
                         (zones (list master-zone))
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

               (service zabbix-server-service-type)
               (service zabbix-agentd-service-type)

               (postgresql-service)

               (service php-fpm-service-type
                        (php-fpm-configuration
                         (file
                          (local-file (string-append %source-dir "/php-fpm")))
                         (php php-with-bcmath)))

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
                                                        (export-all? #t))))))
                                       (listen '("80" "443 ssl"))
                                       (ssl-certificate (letsencrypt-certificate "cgit.duckdns.org"))
                                       (ssl-certificate-key (letsencrypt-key "cgit.duckdns.org")))))))

               (service certbot-service-type
                        (certbot-configuration
                         (email "go.wigust@gmail.com")
                         (certificates
                          (list
                           (certificate-configuration
                            (domains '("cgit.duckdns.org"))
                            (deploy-hook %nginx-deploy-hook))
                           (certificate-configuration
                            (domains '("guix.duckdns.org"))
                            (deploy-hook %nginx-deploy-hook))
                           (certificate-configuration
                            (domains '("anongit.duckdns.org"))
                            (deploy-hook %nginx-deploy-hook))
                           (certificate-configuration
                            (domains '("alerta.duckdns.org"))
                            (deploy-hook %nginx-deploy-hook))))))

               (extra-special-file "/bin/sh"
                                   (file-append bash "/bin/sh"))
               (extra-special-file "/usr/bin/env"
                                   (file-append coreutils "/bin/env"))

               (custom-desktop-services
                #:tor-config-file (string-append %source-dir "/torrc"))))

    (setuid-programs (cons (file-append ubridge "/bin/ubridge")
                           %setuid-custom-programs))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

;;; magnolia.scm ends here
