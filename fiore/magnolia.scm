;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu)
             ((guix ui) #:select (make-user-module))
             (guix profiles)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 textual-ports))

(use-service-modules certbot cuirass cups databases desktop dns mail
networking rsync shepherd spice ssh sysctl version-control
virtualization web xorg cgit wigust-monitoring)

(use-package-modules admin android bash bootloaders certs cryptsetup
cups databases dns file fonts fontutils freedesktop gnome gnupg linux
mail ncurses networking monitoring ratpoison readline rsync pulseaudio
screen ssh tmux version-control virtualization web wget xdisorg xorg
zile wigust-php)

(define %source-dir (string-append (getenv "HOME") "/dotfiles/fiore"))


;;;
;;; General
;;;

(define (cartesian-product . lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))

(define* (prefix-local-host-aliases #:key
                                    prefixes
                                    host-name
                                    domain
                                    ip-addresses)
  (string-join (map (lambda (x)
                      (string-append (string-join x " ")
                                     "." host-name domain))
                    (cartesian-product ip-addresses prefixes))
               "\n"))

(define (serialize-hosts lst)
  (string-join (map (match-lambda
                      ((ip-address . canonical-hostname)
                       (format #f "~a ~a"
                               ip-address canonical-hostname)))
                    lst)
               "\n"))


;;;
;;; Firewall service
;;;

(define start-firewall
  #~(let ((iptables
           (lambda (str)
             (zero? (system (string-join `(,#$(file-append iptables
                                                           "/sbin/iptables")
                                           ,str) " "))))))
      (format #t "Install iptables rules.~%")
      (and
       ;; Rules to throttle malicious SSH connection attempts.  This will
       ;; allow at most 3 connections per minute from any host, and will block
       ;; the host for another minute if this rate is exceeded.  Taken from
       ;; <http://www.la-samhna.de/library/brutessh.html#3>.
       #;(iptables "-A INPUT -p tcp --dport 22 -m state \
  --state NEW -m recent --set --name SSH -j ACCEPT")
       #;(iptables "-A INPUT -p tcp --dport 22 -m recent \
  --update --seconds 60 --hitcount 4 --rttl \
  --name SSH -j LOG --log-prefix SSH_brute_force")
       #;(iptables "-A INPUT -p tcp --dport 22 -m recent \
  --update --seconds 60 --hitcount 4 --rttl --name SSH -j DROP")

       ;; TODO: Map over a list of ports
       #;(iptables "-A INPUT -p tcp --dport 80 -s 192.168.0.0/16 -j ACCEPT") ; web
       #;(iptables "-A INPUT -p tcp --dport 445 -s 192.168.0.0/16 -j ACCEPT") ; smb
       #;(iptables "-A INPUT -p tcp --dport 3389 -s 192.168.0.0/16 -j ACCEPT") ; rdp
       #;(iptables "-A INPUT -p tcp --dport 80 -s 127.0.0.0/8 -j ACCEPT")
       #;(iptables "-A INPUT -p tcp --dport 80 -j DROP")

       ;; Rules to throttle HTTP connection redirections.  Taken from
       ;; <https://www.opennet.ru/tips/2999_iptables_block_tor.shtml>.
       (iptables "-A INPUT -p tcp --sport 443 --tcp-flags RST RST -j DROP")
       (iptables "-A INPUT -p tcp --sport 80 -m string \
--string \"Location: http://warning.rt.ru\" --algo bm -j DROP")
       (iptables "-A INPUT -p tcp --sport 80 -m string \
--string \"Location: http://promo.nw.rt.ru\" --algo bm -j DROP"))))

(define firewall-service
  ;; The "firewall".  Make it a Shepherd service because as an activation
  ;; script it might run too early, before the Netfilter modules can be
  ;; loaded for some reason.
  (simple-service 'firewall shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(firewall))
                    (requirement '())
                    (start #~(lambda _
                               #$start-firewall))
                    (respawn? #f)
                    (stop #~(lambda _
                              (zero?
                               (system* #$(file-append iptables
                                                       "/sbin/iptables")
                                        "-F"))))))))


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
;;; guix-daemon
;;;

(define %guix-daemon-config
  (guix-configuration
   ;; Import keys
   ;; $ wget https://git.savannah.gnu.org/cgit/guix/maintenance.git/plain/hydra/keys/guix/berlin.guixsd.org-export.pub
   ;; # “guix archive --authorize < berlin.guixsd.org-export.pub”
   (substitute-urls '("https://berlin.guixsd.org"
                      "https://mirror.hydra.gnu.org"
                      "https://hydra.gnu.org"))
   ;; (authorized-keys '())
   (max-silent-time 7200)
   (timeout (* 4 max-silent-time))

   (extra-options '("--max-jobs=6" "--cores=3"
                    ;; Origin <https://lists.gnu.org/archive/html/guix-devel/2018-07/msg00310.html>.
                    "--gc-keep-derivations=yes"
                    "--gc-keep-outputs=yes"))))


;;;
;;; Desktop services
;;;

(define 20-intel.conf "
# Fix tearing on intel
# https://wiki.archlinux.org/index.php/Intel_Graphics
# https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"AccelMethod\"  \"sna\"
   Option      \"SwapbuffersWait\" \"true\"
   Option      \"TearFree\" \"true\"
EndSection
")

(define %custom-desktop-services
  ;; Desktop service provides:
  ;;
  ;; Disable NETWORK-MANAGER-SERVICE-TYPE
  ;; Fix tearing on Intel video card 20-INTEL.CONF
  ;; Files /bin/sh and /usr/bin/env
  ;;
  ;; Inspired by https://lists.gnu.org/archive/html/help-guix/2016-01/msg00064.html
  (modify-services (remove (lambda (service)
                             (or (eq? (service-kind service)
                                      network-manager-service-type)))
                           %desktop-services)
    (guix-service-type config => %guix-daemon-config)
    (special-files-service-type config => `(("/bin/sh"
                                             ,(file-append
                                               bash "/bin/sh"))
                                            ("/usr/bin/env"
                                             ,(file-append
                                               coreutils "/bin/env"))))
    (slim-service-type config => (slim-configuration
                                  (inherit config)
                                  (startx
                                   (xorg-start-command
                                    #:configuration-file
                                    (xorg-configuration-file
                                     #:extra-config (list 20-intel.conf))))
                                  (auto-login? #f)
                                  (default-user "natsu")))))


;;;
;;; NGINX
;;;

(define letsencrypt-certificate
  (cut string-append "/etc/letsencrypt/live/" <> "/fullchain.pem"))

(define letsencrypt-key
  (cut string-append "/etc/letsencrypt/live/" <> "/privkey.pem"))

(define* (ssh-forward #:key port host)
  (list "resolver 80.80.80.80;"
        (string-append "set $target localhost:" (number->string port) ";")
        "proxy_pass http://$target;"
        (format #f "proxy_set_header Host ~a;" host)
        "proxy_set_header X-Real-IP $remote_addr;"
        "proxy_set_header X-Forwarded-for $remote_addr;"
        "proxy_connect_timeout 300;"))

(define %file-share-configuration-nginx
  (nginx-configuration
   (server-blocks
    (list (nginx-server-configuration
           (server-name '("www.magnolia.local"))
           (listen '("80"))
           (root "/srv/share")
           (ssl-certificate #f)
           (ssl-certificate-key #f))))))

(define zabbix-nginx-service
  (simple-service 'zabbix-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("alerta.duckdns.org"))
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
          (ssl-certificate-key (letsencrypt-key "alerta.duckdns.org"))))))

(define cups-nginx-service
  (simple-service 'torrent-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("print.magnolia.local"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:631;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define torrent-nginx-service
  (simple-service 'torrent-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("torrent.magnolia.local"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:9091;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define intr-hms-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("hms-billing.majordomo.ru"))
          (listen '("443"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (list "resolver 80.80.80.80;"
                                        (string-append "set $target localhost:" (number->string 16280) ";")
                                        "proxy_pass https://$target;"
                                        (format #f "proxy_set_header Host ~a;" "hms-billing.majordomo.ru")
                                        "proxy_set_header X-Real-IP $remote_addr;"
                                        "proxy_set_header X-Forwarded-for $remote_addr;"
                                        "proxy_connect_timeout 300;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define intr-rpc-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("rpc-mj.intr"))
          (listen '("443"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (list "resolver 80.80.80.80;"
                                        (string-append "set $target localhost:" (number->string 16280) ";")
                                        "proxy_pass https://$target;"
                                        (format #f "proxy_set_header Host ~a;" "rpc-mj.intr")
                                        "proxy_set_header X-Real-IP $remote_addr;"
                                        "proxy_set_header X-Forwarded-for $remote_addr;"
                                        "proxy_connect_timeout 300;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define intr-alerta-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("alerta.intr"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (ssh-forward #:port 16180
                                               #:host "alerta.intr")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define intr-web-alerta-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("web.alerta.intr"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (ssh-forward #:port 16480
                                               #:host "web.alerta.intr")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define intr-zabbix-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("zabbix.intr"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (ssh-forward #:port 15081
                                               #:host "zabbix.intr")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define cerb-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("cerberus.intr"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (ssh-forward #:port 15080
                                               #:host "cerberus.intr")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define grafana-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("grafana.intr"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (ssh-forward #:port 16080
                                               #:host "grafana.intr")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define guix-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("guix.duckdns.org"))
          (listen '("80" "443 ssl"))
          (ssl-certificate (letsencrypt-certificate "guix.duckdns.org"))
          (ssl-certificate-key (letsencrypt-key "guix.duckdns.org"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:3000;")))))))))

(define tail-guix-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("guix.tail.local"))
          (listen '("80"))
          (ssl-certificate #f)
          (ssl-certificate-key #f)
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (ssh-forward #:port 19080
                                               #:host "guix.tail.local")))))))))

(define tail-cgit-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("cgit.tail.local"))
          (listen '("80"))
          (ssl-certificate #f)
          (ssl-certificate-key #f)
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (ssh-forward #:port 19080
                                               #:host "cgit.tail.local")))))))))

(define local-esxi-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("esxi.local"))
          (listen '("443"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body (list "resolver 80.80.80.80;"
                                        (string-append "set $target localhost:" (number->string 17443) ";")
                                        "proxy_pass https://$target;"
                                        (format #f "proxy_set_header Host ~a;" "192.168.125.22")
                                        "proxy_set_header X-Real-IP $remote_addr;"
                                        "proxy_set_header X-Forwarded-for $remote_addr;"
                                        "proxy_connect_timeout 300;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define natsu-nginx-service
  (simple-service 'natsu-nginx nginx-service-type
   (list (nginx-server-configuration
         (server-name '("natsu.magnolia.local"))
         (listen '("80"))
         (root "/home/natsu/public_html")
         (ssl-certificate #f)
         (ssl-certificate-key #f)))))

(define %cgit-configuration-nginx-custom
  (nginx-server-configuration
   (inherit %cgit-configuration-nginx)
   (locations (append (nginx-server-configuration-locations %cgit-configuration-nginx)
                      (list (git-http-nginx-location-configuration
                             (git-http-configuration
                              (export-all? #t))))))
   (listen '("80" "443 ssl"))
   (ssl-certificate (letsencrypt-certificate "cgit.duckdns.org"))
   (ssl-certificate-key (letsencrypt-key "cgit.duckdns.org"))
   (server-name '("cgit.duckdns.org"))))

(define anongit-nginx-service
  (simple-service 'anongit-nginx nginx-service-type
   (list (nginx-server-configuration
          (listen '("80" "443 ssl"))
          (server-name '("anongit.duckdns.org"))
          (root "/srv/git")
          (ssl-certificate (letsencrypt-certificate "anongit.duckdns.org"))
          (ssl-certificate-key (letsencrypt-key "anongit.duckdns.org"))
          (locations
           (list
            (git-http-nginx-location-configuration
             (git-http-configuration (uri-path "/")
                                     (export-all? #t)))))))))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))


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

    (groups (cons* (user-group (name "adbusers"))
                   (user-group (name "guix-offload"))
                   (user-group (name "telegraf") (system? #t))
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
                  %base-user-accounts))

    ;; Create a /etc/hosts file with aliases for "localhost"
    ;; and "mymachine", as well as for Facebook servers.
    (hosts-file (plain-file "hosts"
                            (string-append (local-host-aliases host-name)
                                           (prefix-local-host-aliases #:prefixes '("cgit" "esxi" "git" "guix" "www"
                                                                                   "natsu" "torrent" "print"
                                                                                   "zabbix")
                                                                      #:host-name host-name
                                                                      #:domain ".local"
                                                                      #:ip-addresses (list ip-address))
                                           "\n"
                                           (prefix-local-host-aliases #:prefixes '("cgit" "guix")
                                                                      #:host-name "tail"
                                                                      #:domain ".local"
                                                                      #:ip-addresses (list ip-address))
                                           "\n"
                                           (prefix-local-host-aliases #:prefixes '("cgit" "anongit" "guix" "alerta" "weblog")
                                                                      #:host-name "duckdns"
                                                                      #:domain ".org"
                                                                      #:ip-addresses (list ip-address))
                                           "\n"
                                           (prefix-local-host-aliases #:prefixes '("alerta" "cerberus" "grafana" "rpc-mj"
                                                                                   "web.alerta" "zabbix")
                                                                      #:host-name ""
                                                                      #:domain "intr"
                                                                      #:ip-addresses (list ip-address))
                                           "\n\n"
                                           (serialize-hosts '(("192.168.100.1" . "r1.local")
                                                              ("192.168.105.1" . "r2.local")
                                                              ("192.168.105.120" . "magnolia")
                                                              ("192.168.105.112" . "clover")
                                                              ("192.168.105.120" . "hms-billing.majordomo.ru")))
                                           "\n\n" %facebook-host-aliases)))

    ;; Lightweight desktop with custom packages from guix-wigust
    (packages (let ((module (make-user-module '((guix profiles) (gnu)))))
                (save-module-excursion
                 (lambda _
                   (set-current-module module)
                   (map manifest-entry-item
                        (manifest-entries (load (string-append %source-dir
                                                               "/manifests/fiore.scm"))))))))

    (services (cons* firewall-service
                     custom-networking-service
                     (static-networking-service "enp6s0" ip-address
                                                #:netmask "255.255.255.0"
                                                #:gateway "192.168.105.1"
                                                ;; See <http://www.freenom.world>.
                                                #:name-servers '("80.80.80.80"
                                                                 "80.80.81.81"))

                     (service sysctl-service-type
                              (sysctl-configuration
                               (settings '(("net.ipv4.ip_forward" . "1")
                                           ("net.ipv4.conf.all.accept_redirects" . "1")
                                           ("net.ipv4.conf.all.send_redirects" . "1")))))
                     ;; 92 bytes from switch.local (192.168.105.1): Redirect Host

                     (service ddclient-service-type)

                     (service libvirt-service-type
                              (libvirt-configuration
                               (unix-sock-group "libvirt")
                               (tls-port "16555")))

                     (service virtlog-service-type)

                     (service openssh-service-type
                              (openssh-configuration
                               (permit-root-login #t)
                               (x11-forwarding? #t)))

                     (service cups-service-type
                              (cups-configuration
                               (location-access-controls
                                (list (location-access-control
                                       (path "/")
                                       (access-controls '("Order allow,deny"
                                                          "Allow localhost"
                                                          "Allow 192.168.0.*")))
                                      (location-access-control
                                       (path "/admin")
                                       (access-controls '("Order allow,deny"
                                                          "Allow localhost")))
                                      (location-access-control
                                       (path "/admin/conf")
                                       (access-controls '("Order allow,deny"
                                                          "AuthType Basic"
                                                          "Require user @SYSTEM"
                                                          "Allow localhost")))))
                               (web-interface? #t) ; LibreJS could block JS
                               (extensions (list cups-filters hplip-minimal))))

                     (dovecot-service
                      #:config (dovecot-configuration
                                (listen '("127.0.0.1"))
                                (disable-plaintext-auth? #f)
                                (mail-location
                                 (string-append "maildir:~/Maildir"
                                                ":INBOX=~/Maildir/INBOX"
                                                ":LAYOUT=fs"))))

                     (service guix-publish-service-type
                              (guix-publish-configuration
                               (host "0.0.0.0") (port 3000)))

                     (service git-daemon-service-type
                              (git-daemon-configuration (user-path "")
                                                        (export-all? #t)))

                     (service rsync-service-type)

                     (service fcgiwrap-service-type)

                     (tor-service (local-file (string-append %source-dir "/torrc")))

                     (service zabbix-server-service-type)
                     (service zabbix-agentd-service-type)

                     (postgresql-service)

                     (service php-fpm-service-type
                              (php-fpm-configuration
                               (file (local-file "/home/natsu/src/guix-wigust/php-fpm"))
                               (php php-with-bcmath)))

                     (service nginx-service-type
                              %file-share-configuration-nginx)

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
                               (nginx (list %cgit-configuration-nginx-custom))))

                     zabbix-nginx-service
                     intr-zabbix-publish-nginx-service
                     intr-alerta-publish-nginx-service
                     intr-web-alerta-publish-nginx-service
                     intr-hms-publish-nginx-service
                     intr-rpc-publish-nginx-service
                     cerb-publish-nginx-service
                     guix-publish-nginx-service
                     anongit-nginx-service
                     cups-nginx-service
                     torrent-nginx-service
                     natsu-nginx-service
                     grafana-publish-nginx-service
                     ;; local-esxi-publish-nginx-service
                     tail-guix-nginx-service
                     tail-cgit-nginx-service

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

                     (simple-service 'adb udev-service-type
                                     (list android-udev-rules))

                     (xfce-desktop-service)

                     %custom-desktop-services))

    (setuid-programs (cons* (file-append fping "/sbin/fping")
                            (file-append ubridge "/bin/ubridge")
                            %setuid-programs))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

;;; system-magnolia.scm ends here
