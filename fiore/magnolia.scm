;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(define-module (fiore magnolia)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (fiore manifests fiore))

(use-service-modules certbot cups desktop dns mail networking rsync
shepherd spice ssh version-control virtualization web xorg cgit)

(use-package-modules admin android bash bootloaders certs cryptsetup cups
databases dns file fonts fontutils freedesktop gnome gnupg linux mail
ncurses networking ratpoison readline rsync pulseaudio screen ssh tmux
version-control virtualization web wget xdisorg xorg zile)

(define %source-dir (string-append (getenv "HOME") "/dotfiles/fiore"))


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

   (extra-options '("--max-jobs=6" "--cores=3"))))


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

(define %file-share-configuration-nginx
  (nginx-configuration
   (server-blocks
    (list (nginx-server-configuration
           (server-name '("www.magnolia.local"))
           (listen '("80"))
           (root "/srv/share")
           (ssl-certificate #f)
           (ssl-certificate-key #f))))))

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

(define zabbix-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("zabbix.intr"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("resolver 80.80.80.80;"
                                    "set $target localhost:15081;"
                                    "proxy_pass http://$target;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define cerb-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("cerberus.intr"))
          (listen '("80"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("resolver 80.80.80.80;"
                                    "set $target localhost:15080;"
                                    "proxy_pass http://$target;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define guix-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("guix.duckdns.org"))
          (listen '("80" "443 ssl"))
          (ssl-certificate
           "/etc/letsencrypt/live/guix.duckdns.org/fullchain.pem")
          (ssl-certificate-key
           "/etc/letsencrypt/live/guix.duckdns.org/privkey.pem")
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:3000;")))))))))

(define natsu-nginx-service
  (simple-service 'natsu-nginx nginx-service-type
   (list (nginx-server-configuration
         (server-name '("natsu.magnolia.local"))
         (listen '("80"))
         (root "/home/natsu/public_html")
         (ssl-certificate #f)
         (ssl-certificate-key #f)))))

(define %cgit-configuration-nginx-body
    (list "fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
          "fastcgi_param PATH_INFO $uri;"
          "fastcgi_param QUERY_STRING $args;"
          "fastcgi_param HTTP_HOST $server_name;"
          "fastcgi_pass 127.0.0.1:9000;"))

(define %cgit-configuration-nginx-custom
  (nginx-server-configuration
   (inherit %cgit-configuration-nginx)
   (listen '("80" "443 ssl"))
   (ssl-certificate
    "/etc/letsencrypt/live/cgit.duckdns.org/fullchain.pem")
   (ssl-certificate-key
    "/etc/letsencrypt/live/cgit.duckdns.org/privkey.pem")
   (server-name '("cgit.duckdns.org"))))

(define anongit-nginx-service
  (simple-service 'anongit-nginx nginx-service-type
   (list (nginx-server-configuration
          (listen '("80" "443 ssl"))
          (server-name '("anongit.duckdns.org"))
          (root "/srv/git")
          (ssl-certificate
           "/etc/letsencrypt/live/anongit.duckdns.org/fullchain.pem")
          (ssl-certificate-key
           "/etc/letsencrypt/live/anongit.duckdns.org/privkey.pem")
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
;;; Support functions
;;;

;; Origin <https://stackoverflow.com/a/20591545>.
(define (cartesian-product . lists)
  (fold-right (lambda (xs ys)
                (append-map (lambda (x)
                              (map (lambda (y)
                                     (cons x y))
                                   ys))
                            xs))
              '(())
              lists))


;;;
;;; /etc/hosts
;;;

(define (prefix-local-host-aliases prefix host-name domain ip-addresses)
  (string-join (map (lambda (x)
                      (string-append (string-join x " ") "." host-name domain))
                    (cartesian-product ip-addresses prefix))
               "\n"))

(define %magnolia-ip-address "192.168.105.120")


;;;
;;; Operating system.
;;;

(define %system-magnolia
  (operating-system
    (host-name "magnolia")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")))

    (file-systems (cons* (file-system
                           (device "magnolia-root")
                           (title 'label)
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device "/dev/sda1")
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "magnolia-data")
                           (title 'label)
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

    (groups (cons
             (user-group (name "adbusers"))
             %base-groups))

    (users (cons (user-account
                  (name "natsu")
                  (uid 1000)
                  (comment "Oleg Pykhalov")
                  (group "users")
                  (supplementary-groups '("wheel"
                                          "audio" "video"
                                          "lpadmin" "lp"
                                          "adbusers" "libvirt"))
                  (home-directory "/home/natsu"))
                 %base-user-accounts))

    (hosts-file
     ;; Create a /etc/hosts file with aliases for "localhost"
     ;; and "mymachine", as well as for Facebook servers.
     (plain-file "hosts"
                 (string-append (local-host-aliases host-name)
                                (prefix-local-host-aliases
                                 '("cgit" "git" "guix" "www"
                                   "natsu" "torrent" "print")
                                 host-name ".local"
                                 (list %magnolia-ip-address))
                                "\n"
                                (prefix-local-host-aliases
                                 '("cgit" "anongit" "guix")
                                 "duckdns" ".org"
                                 (list %magnolia-ip-address))
                                "\n"
                                (prefix-local-host-aliases
                                 '("zabbix" "cerberus")
                                 "" "intr"
                                 (list %magnolia-ip-address))
                                "\n\n" %facebook-host-aliases)))

    ;; Lightweight desktop with custom packages from guix-wigust
    (packages %fiore-packages)

    (services (cons* firewall-service
                     (static-networking-service "enp6s0"
                                                %magnolia-ip-address
                                                #:netmask "255.255.255.0"
                                                #:gateway "192.168.105.1"
                                                ;; See <http://www.freenom.world>.
                                                #:name-servers '("80.80.80.80"
                                                                 "80.80.81.81"))

                     (service libvirt-service-type
                              (libvirt-configuration
                               (unix-sock-group "libvirt")
                               (tls-port "16555")))

                     (service virtlog-service-type)

                     (service openssh-service-type)

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
                               (extensions (list cups-filters hplip))))

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

                     (service nginx-service-type
                              %file-share-configuration-nginx)

                     (spice-vdagent-service)

                     ;; TODO: Upload to Guix
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
                                                   "https://anongit.duckdns.org"))
                               (nginx (list %cgit-configuration-nginx-custom))))

                     zabbix-publish-nginx-service
                     cerb-publish-nginx-service
                     guix-publish-nginx-service
                     anongit-nginx-service
                     cups-nginx-service
                     torrent-nginx-service
                     natsu-nginx-service

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
                                  (deploy-hook %nginx-deploy-hook))))))

                     (simple-service 'adb udev-service-type
                                     (list android-udev-rules))

                     (xfce-desktop-service)

                     %custom-desktop-services))

    (setuid-programs (cons (file-append fping "/sbin/fping")
                           %setuid-programs))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

%system-magnolia

;;; system-magnolia.scm ends here
