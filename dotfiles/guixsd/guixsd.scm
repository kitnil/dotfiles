(use-modules (gnu)
             (gnu services shepherd)
             (gnu services)
             (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-26))

(use-package-modules admin audio android bittorrent linux ssh suckless xdisorg)

(use-service-modules admin dbus desktop docker dns networking sound
                     xorg ssh web cgit version-control certbot
                     monitoring databases mail vpn)

;; Third-party modules
(use-modules (config)
             (packages admin)
             (packages web)
             (services autofs)
             (services bittorrent)
             (services nix)
             (services autossh)
             (services intel)
             (services kresd)
             (services jenkins)
             (services tftp)
             (services openvpn)
             (services webssh))

;; Fix Jenkins in Docker group
(module-set! (resolve-module '(gnu packages admin)) 'shepherd shepherd-patched)

(define 30-multihead.conf "\
Section \"Monitor\"
    Identifier  \"HDMI1\"
    Option      \"Primary\" \"true\"
EndSection

Section \"Monitor\"
    Identifier  \"HDMI3\"
    Option      \"RightOf\" \"HDMI1\"
EndSection")


;;;
;;; Certbot
;;;

(define letsencrypt-certificate
  (cut string-append "/etc/letsencrypt/live/" <> "/fullchain.pem"))

(define letsencrypt-key
  (cut string-append "/etc/letsencrypt/live/" <> "/privkey.pem"))

(define %certbot-hosts
  (list "cgit.duckdns.org"
        "githunt.wugi.info"
        "homer.wugi.info"
        "guix.duckdns.org"
        "zabbix.wugi.info"
        "jenkins.wugi.info"
        "torrent.wugi.info"
        "webssh.wugi.info"))


;;;
;;; NGINX
;;;

(define %nginx-certbot
  (nginx-location-configuration
   (uri "/.well-known")
   (body '("root /var/www;"))))

(define %mtls
  (list #~(format #f "ssl_client_certificate ~a;"
                  #$(local-file "/home/oleg/src/ssl/ca.pem"))
        "ssl_verify_client on;"))

(define* (proxy host port
                #:key
                (ssl? #f)
                (ssl-target? #f)
                (ssl-key? #f)
                (well-known? #t)
                (target #f)
                (sub-domains? #f)
                (mtls? #f))
  (nginx-server-configuration
   (server-name (if sub-domains?
                    (list (string-append sub-domains?
                                         (string-join (string-split host #\.)
                                                      "\\.")
                                         "$"))
                    (list host (string-append "www." host))))
   (locations (delete #f
                      (list (nginx-location-configuration
                             (uri "/")
                             (body (list "resolver 80.80.80.80;"
                                         (string-append "set $target "
                                                        (or target "127.0.0.1")
                                                        ":" (number->string port) ";")
                                         (format #f "proxy_pass ~a://$target;" (if ssl-target? "https" "http"))
                                         (if sub-domains?
                                             "proxy_set_header Host $http_host;"
                                             (format #f "proxy_set_header Host ~a;" host))
                                         "proxy_set_header X-Forwarded-Proto $scheme;"
                                         "proxy_set_header X-Real-IP $remote_addr;"
                                         "proxy_set_header X-Forwarded-for $remote_addr;"
                                         "proxy_connect_timeout 300;"
                                         "client_max_body_size 0;")))
                            (and well-known?
                                 (nginx-location-configuration
                                  (uri "/.well-known")
                                  (body '("root /var/www;")))))))
   (listen (if ssl?
               (list "443 ssl")
               (list "80")))
   (ssl-certificate (if ssl-key? (letsencrypt-certificate host) #f))
   (ssl-certificate-key (if ssl-key? (letsencrypt-key host) #f))
   (raw-content (if mtls? %mtls '()))))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define %nginx-server-blocks
  (list (nginx-server-configuration
         (server-name '("www.tld"))
         (listen '("80"))
         (root "/srv/share"))
        (nginx-server-configuration
         (server-name '("techinfo.intr"))
         (listen '("80"))
         (root "/var/www/techinfo.intr"))
        (nginx-server-configuration
         (server-name '("iso.wugi.info"))
         (listen '("80"))
         (root "/srv/iso"))
        (nginx-server-configuration
         (server-name '("homer.wugi.info"))
         (listen '("443 ssl"))
         (root (file-append homer "/share/homer"))
         (locations
          (list (nginx-location-configuration
                 (uri "/.well-known")
                 (body '("root /var/www;")))))
         (ssl-certificate (letsencrypt-certificate "homer.wugi.info"))
         (ssl-certificate-key (letsencrypt-key "homer.wugi.info"))
         (raw-content %mtls))
        (nginx-server-configuration
         (server-name '("texinfo.tld"))
         (listen '("80"))
         (root "/var/www/texinfo"))

        ;; (proxy "hms.majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "www.majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "hms-dev.intr" 7777 #:ssl? #f)
        ;; (proxy "hms.majordomo.ru" 7777 #:ssl? #f)
        (nginx-server-configuration
         (server-name '("hms.majordomo.ru" "hms-dev.intr" "www.majordomo.ru" "majordomo.ru"))
         (listen '("80" "443 ssl"))
         (ssl-certificate "/etc/tls/hms.majordomo.ru.pem")
         (ssl-certificate-key "/etc/tls/hms.majordomo.ru.key")
         (locations (list (nginx-location-configuration
                           (uri "/")
                           (body (list "root /home/oleg/majordomo/hms/frontend-app/public;"
                                       "proxy_set_header Access-Control-Allow-Origin *;"
                                       "index  index.html;"
                                       "try_files $uri $uri/ /index.html;"
                                       ;; "proxy_pass http://127.0.0.1:3000;"
                                       ;; "proxy_set_header Host hms.majordomo.ru;"
                                       ;; "proxy_set_header X-Forwarded-Proto $scheme;"
                                       ;; "proxy_set_header X-Real-IP $remote_addr;"
                                       ;; "proxy_set_header X-Forwarded-for $remote_addr;"
                                       ;; "proxy_connect_timeout 300;"
                                       ;; "client_max_body_size 0;"
                                       ))))))
        (nginx-server-configuration
         (server-name '("hms-billing-dev.intr"))
         (listen '("80" "443 ssl"))
         (ssl-certificate "/etc/tls/hms.majordomo.ru.pem")
         (ssl-certificate-key "/etc/tls/hms.majordomo.ru.key")
         ;; (root "/home/oleg/majordomo/hms/staff-frontend-app/public")
         (locations (list (nginx-location-configuration
                           (uri "/")
                           (body (list "proxy_set_header Access-Control-Allow-Origin *;"
                                       ;; "rewrite     ^   https://$server_name$request_uri?;"
                                       "root   /home/oleg/majordomo/hms/staff-frontend-app/public;"
                                       "index  index.html;"
                                       "try_files $uri $uri/ /index.html;"
                                       ;; "proxy_pass http://127.0.0.1:3001;"
                                       ;; "proxy_set_header Host hms.majordomo.ru;"
                                       ;; "proxy_set_header X-Forwarded-Proto $scheme;"
                                       ;; "proxy_set_header X-Real-IP $remote_addr;"
                                       ;; "proxy_set_header X-Forwarded-for $remote_addr;"
                                       ;; "proxy_connect_timeout 300;"
                                       ;; "client_max_body_size 0;"
                                       ;; "proxy_set_header Access-Control-Allow-Origin *;"
                                       ))))))

;;         (nginx-server-configuration
;;          (server-name '("hms-dev.intr" "hms.majordomo.ru"))
;;          (listen '("80"))
;;          (root "/home/static/hms-frontend")
;;          (raw-content (list "\
;; location / {
;;     proxy_set_header Access-Control-Allow-Origin *;
;;     root   /home/static/hms-frontend;
;;     index  index.html;
;;     try_files $uri $uri/ /index.html;
;; }
;; ")))

        (nginx-server-configuration
         (server-name '("api-dev.intr"))
         (listen '("80"))
         (raw-content (list "\
location / {
    proxy_set_header Access-Control-Allow-Origin *;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-NginX-Proxy true;
    proxy_pass http://localhost:8082/;
    proxy_ssl_session_reuse off;
    proxy_set_header Host $http_host;
    proxy_redirect off;
    proxy_buffer_size 128k;
    proxy_buffers 4 256k;
}
")))
        (nginx-server-configuration
         (server-name '("www.example.com" "example.com"))
         (listen '("80"))
         (raw-content (list "\
location / {
    proxy_set_header Access-Control-Allow-Origin *;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-NginX-Proxy true;
    proxy_pass http://localhost:8080/;
    proxy_ssl_session_reuse off;
    proxy_set_header Host $http_host;
    proxy_redirect off;
    proxy_buffer_size 128k;
    proxy_buffers 4 256k;
}
")))

        (nginx-server-configuration
         (inherit %webssh-configuration-nginx)
         (server-name '("webssh.wugi.info"))
         (listen '("443 ssl"))
         (ssl-certificate (letsencrypt-certificate "webssh.wugi.info"))
         (ssl-certificate-key (letsencrypt-key "webssh.wugi.info"))
         (locations
          (cons (nginx-location-configuration
                 (uri "/.well-known")
                 (body '("root /var/www;")))
                (nginx-server-configuration-locations %webssh-configuration-nginx))))

        (proxy "cups.tld" 631)
        (proxy "torrent.wugi.info" 9091 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "jenkins.wugi.info" 8090 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "githunt.wugi.info" 3000 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "guix.duckdns.org" 5556 #:ssl? #t)
        (proxy "guix.wugi.info" 5556)
        (proxy "pykhaloff.ddns.net" 443
               #:target "192.168.100.5"
               #:ssl? #t
               #:ssl-target? #t
               #:ssl-key? #f)
        ((lambda* (host #:key
                  (ssl? #f)
                  (ssl-target? #f)
                  (target #f)
                  (sub-domains? #f))
          (nginx-server-configuration
           (server-name (if sub-domains?
                            (list (string-append sub-domains?
                                                 (string-join (string-split host #\.)
                                                              "\\.")
                                                 "$"))
                            (list host (string-append "www." host))))
           (locations (delete #f
                              (list (nginx-location-configuration
                                     (uri "/api/queue")
                                     (body (list "return 404;")))
                                    (nginx-location-configuration
                                     (uri "/")
                                     (body (list "resolver 80.80.80.80;"
                                                 (string-append "set $target "
                                                                "ci.guix.gnu.org"
                                                                ":" (number->string 80) ";")
                                                 (format #f "proxy_pass ~a://$target;" "http")
                                                 (if sub-domains?
                                                     "proxy_set_header Host $http_host;"
                                                     (format #f "proxy_set_header Host ~a;" "ci.guix.gnu.org"))
                                                 "proxy_set_header X-Forwarded-Proto $scheme;"
                                                 "proxy_set_header X-Real-IP $remote_addr;"
                                                 "proxy_set_header X-Forwarded-for $remote_addr;"
                                                 "proxy_connect_timeout 300;"
                                                 "client_max_body_size 0;"
                                                 ;; https://qasseta.ru/q/100/368287/cloudfront-%D0%BA%D0%B0%D0%BA-%D0%BD%D0%B0%D1%81%D1%82%D1%80%D0%BE%D0%B8%D1%82%D1%8C-%D0%BE%D0%B1%D1%80%D0%B0%D1%82%D0%BD%D1%8B%D0%B9-%D0%BF%D1%80%D0%BE%D0%BA%D1%81%D0%B8-%D1%81%D0%B5%D1%80%D0%B2%D0%B5%D1%80-%D0%BD%D0%B0-%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%BC-%D0%B2%D0%B5%D0%B1-%D1%81%D0%B0%D0%B9%D1%82%D0%B5-%D0%BE%D0%B1%D1%81%D0%BB%D1%83%D0%B6%D0%B8%D0%B2%D0%B0%D1%8E%D1%89%D0%B5%D0%BC-%D1%80%D0%B0%D1%81%D0%BF%D1%80%D0%BE%D1%81%D1%82%D1%80%D0%B0%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BE%D1%82-s3
                                                 ;; "proxy_cache_bypass $http_upgrade;"
                                                 ;; "proxy_set_header Connection 'upgrade';"
                                                 ;; "proxy_set_header Upgrade $http_upgrade;"
                                                 ;; "proxy_http_version 1.1;"
                                                 ))))))
           (listen (if ssl?
                       (list "443 ssl")
                       (list "80")))
           (ssl-certificate (if ssl?
                                (letsencrypt-certificate host)
                                #f))
           (ssl-certificate-key (if ssl?
                                    (letsencrypt-key host)
                                    #f))))
         "cuirass.wugi.info")))

(define %zabbix-nginx-configuration
  (list
   (nginx-server-configuration
    (inherit %zabbix-front-end-configuration-nginx)
    (server-name '("zabbix.wugi.info"))
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
    (ssl-certificate-key (letsencrypt-key "zabbix.wugi.info"))
    (raw-content %mtls))))


;;;
;;; Entryp point
;;;

(define %slim-theme
  (or (and=> (current-filename)
             (lambda (file)
               (string-append (dirname (dirname file))
                              "/fiore/modules/slim-artwork.scm")))
      "/home/oleg/src/dotfiles/fiore/modules/slim-artwork.scm"))

;; TODO: Get rid of full path
(define %hardware-file
  (or (and=> (current-filename)
             (lambda (file)
               (string-append (dirname file) "/hardware/guixsd.scm")))
      "/home/oleg/src/dotfiles/guixsd/hardware/guixsd.scm"))

(define %guix-daemon-config
  (guix-configuration
   ;; (timeout (* 4 max-silent-time))
   (extra-options '(;; "--max-jobs=6" "--cores=3"
                    ;; "--gc-keep-derivations=yes"
                    ;; "--gc-keep-outputs=yes"
                    "--cache-failures"))))

(define %system-guixsd
  (let ((base-system (load %hardware-file)))
    (operating-system
      (inherit base-system)
      (kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"))
      (packages %my-system-packages)

      (groups (cons* (user-group (name "nixbld")
                                 (id 30100))
                     (user-group (name "adbusers"))
                     (user-group (name "docker")
                                 (system? #t))
                     %base-groups))

      (users (cons* (user-account
                     (name "oleg")
                     (uid 1000)
                     (comment "Oleg Pykhalov")
                     (group "users")
                     (supplementary-groups '("wheel" "adbusers" "audio" "video" "docker" "kvm"))
                     (home-directory "/home/oleg"))
                    (user-account
                     (name "majordomo-ssh-tunnel")
                     (uid 30011)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/majordomo-ssh-tunnel"))
                    (user-account
                     (name "tail-ssh-tunnel")
                     (uid 30015)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/tail-ssh-tunnel"))
                    (user-account
                     (name "spb-zabbix-ssh-tunnel")
                     (uid 30020)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/spb-zabbix-ssh-tunnel"))
                    (user-account
                     (name "oracle-ssh-tunnel")
                     (uid 30021)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/oracle-ssh-tunnel"))
                    (user-account
                     (name "spb")
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/spb"))
                    (append ((lambda* (count #:key
                                        (group "nixbld")
                                        (first-uid 30101)
                                        (shadow shadow))
                               (unfold (cut > <> count)
                                       (lambda (n)
                                         (user-account
                                          (name (format #f "nixbld~a" n))
                                          (system? #t)
                                          (uid (+ first-uid n -1))
                                          (group group)

                                          ;; guix-daemon expects GROUP to be listed as a
                                          ;; supplementary group too:
                                          ;; <http://lists.gnu.org/archive/html/bug-guix/2013-01/msg00239.html>.
                                          (supplementary-groups (list group "kvm"))

                                          (comment (format #f "Nix Build User ~a" n))
                                          (home-directory "/var/empty")
                                          (shell (file-append shadow "/sbin/nologin"))))
                                       1+
                                       1))
                             9)
                            %base-user-accounts)))

      (hosts-file
       (plain-file
        "hosts"
        (string-join
         `(,(string-join '("127.0.0.1 guixsd localhost"
                           "zabbix.wugi.info"
                           "techinfo.intr"
                           "texinfo.tld"
                           "jenkins.wugi.info"
                           "torrent.wugi.info"
                           "iso.wugi.info"
                           "cgit.duckdns.org"
                           "spb"
                           ;; Majordomo
                           ;; "hms-dev.intr"
                           ;; "api-dev.intr"
                           ;; "hms-billing-dev.intr"
                           ))
           "::1 guixsd localhost"

           "78.108.82.157 mjru"

           "192.168.100.1 r1.tld"
           "192.168.100.12 r2.tld"
           "192.168.100.120 cuirass.tld"
           "172.16.100.60 workstation.intr"
           "178.250.247.125 gitlab.mjtest jenkins.mjtest"
           "130.61.95.6 oracle"
           "172.16.100.65 zdetovetskiy.intr"
           "127.0.0.1 example.com"

           "78.108.86.20 r1"
           "78.108.87.99 r2"
           "178.250.246.123 r3"
           "178.250.247.60 r4"
           "")
         "\n")))

      (services (cons* (extra-special-file "/usr/bin/env"
                                           (file-append coreutils "/bin/env"))

                       ;; mount -t fuse and autofs
                       (extra-special-file "/bin/sshfs"
                                           (file-append sshfs "/bin/sshfs"))
                       (extra-special-file "/bin/ssh"
                                           (file-append openssh "/bin/ssh"))

                       ;; “adb” and “fastboot” without root privileges
                       (simple-service 'adb udev-service-type (list android-udev-rules))

                       (simple-service 'udev-kvm-custom udev-service-type
                                       (list (udev-rule
                                              "91-kvm-custom.rules"
                                              (string-append "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"\n"))))

                       (service singularity-service-type)

                       (service ladspa-service-type
                                (ladspa-configuration (plugins (list swh-plugins))))

                       ;; Desktop services
                       (service slim-service-type
                                (slim-configuration
				 ;; (theme %slim-theme) TODO: Fix the theme.
                                 (xorg-configuration
                                  (xorg-configuration
                                   (extra-config (list 20-intel.conf
                                                       30-multihead.conf))))))
                       (screen-locker-service slock)
                       (screen-locker-service xlockmore "xlock")
                       (udisks-service)
                       (upower-service)
                       (accountsservice-service)
                       (colord-service)
                       (geoclue-service)
                       (service polkit-service-type)
                       (elogind-service)
                       (dbus-service)
                       (service ntp-service-type)
                       x11-socket-directory-service
                       (service alsa-service-type)

                       nix-service
                       (kresd-service (local-file "kresd.conf"))

                       openvpn-service

                       ;; TODO:
                       ;; (openvpn-client-service
                       ;;  #:config (openvpn-client-configuration
                       ;;            ;; (dev 'tapvpn)
                       ;;            (auth-user-pass "/etc/openvpn/login.conf")
                       ;;            (remote (list
                       ;;                     ;; 78.108.80.230
                       ;;                     (openvpn-remote-configuration
                       ;;                      (name "vpn-miran.majordomo.ru"))
                       ;;                     ;; 78.108.91.250
                       ;;                     (openvpn-remote-configuration
                       ;;                      (name "vpn-dh.majordomo.ru"))
                       ;;                     ;; 81.95.28.29
                       ;;                     (openvpn-remote-configuration
                       ;;                      (name "vpn-office.majordomo.ru"))))))

                       (service autofs-service-type
                                (autofs-configuration
                                 (config-file (local-file "/etc/autofs/auto.master"))))

                       (service openssh-service-type
                                (openssh-configuration
                                 (x11-forwarding? #t)
                                 (gateway-ports? 'client)
                                 (password-authentication? #f)
                                 (extra-content "\
Match Address 127.0.0.1
PasswordAuthentication yes")))

                       (service certbot-service-type
                                (certbot-configuration
                                 (email "go.wigust@gmail.com")
                                 (certificates
                                  `(,@(map (lambda (host)
                                             (certificate-configuration
                                              (domains (list host))
                                              (deploy-hook %nginx-deploy-hook)))
                                           %certbot-hosts)))))

                       (service nginx-service-type
                                (nginx-configuration
                                 (server-blocks %nginx-server-blocks)))

                       (service gitolite-service-type
                                (gitolite-configuration
                                 (admin-pubkey (local-file "/home/oleg/.ssh/id_rsa.pub"))))

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
                                 (root-title "Personal Cgit")
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
                                                              (nginx-location-configuration
                                                               (uri "/.well-known")
                                                               (body '("root /var/www;"))))))
                                               (listen '("80" "443 ssl"))
                                               (ssl-certificate (letsencrypt-certificate "cgit.duckdns.org"))
                                               (ssl-certificate-key (letsencrypt-key "cgit.duckdns.org")))))))

                       (service tor-service-type
                                (tor-configuration
                                 (config-file (local-file "torrc"))))

                       (service ddclient-service-type)

		       ;; TODO: Move those services.

                       (postgresql-service #:config-file (postgresql-config-file
                                                          (hba-file
                                                           (plain-file "pg_hba.conf"
                                                                       "
local	all	all			trust
host	all	all	127.0.0.1/32    trust
host	all	all	::1/128         trust
host	all	all	172.16.0.0/12   trust"))
                                                          (extra-config '(("listen_addresses" "'0.0.0.0'")))))

                       (service mongodb-service-type)

                        (service php-fpm-service-type
                                (php-fpm-configuration
                                 (timezone "Europe/Moscow")))

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
                                 (extra-options (string-join
                                                 (list ""
                                                       "UserParameter=ssl_cert_check_valid[*], /etc/zabbix/externalscripts/ssl_cert_check.sh valid \"$1\" \"$2\" \"$3\""
                                                       "UserParameter=ssl_cert_check_expire[*], /etc/zabbix/externalscripts/ssl_cert_check.sh expire \"$1\" \"$2\" \"$3\""
                                                       (string-join (cons "UserParameter=ssl_cert_hosts[*], /etc/zabbix/externalscripts/ssl_cert_hosts.sh"
                                                                          %certbot-hosts)))
                                                 "\n"))))

                       (service zabbix-front-end-service-type
                                (zabbix-front-end-configuration
                                 (db-secret-file "/etc/zabbix/zabbix.secret")
                                 (nginx %zabbix-nginx-configuration)))

                       jenkins-service

                       (service docker-service-type)

                       (dovecot-service
                        #:config (dovecot-configuration
                                  (listen '("127.0.0.1"))
                                  (disable-plaintext-auth? #f)
                                  (mail-location
                                   (string-append "maildir:~/Maildir"
                                                  ":INBOX=~/Maildir/INBOX"
                                                  ":LAYOUT=fs"))))

                       tftp-service

                       (service guix-publish-service-type
                                (guix-publish-configuration
                                 (host "0.0.0.0")
                                 (port 5556)))

                       (service (@ (services autossh) autossh-service-type)
                                ((@ (services autossh) autossh-configuration)
                                 (autossh-client-config
                                  (autossh-client-configuration
                                   (hosts (list (autossh-client-host-configuration
                                                 (host "znc.wugi.info")
                                                 (identity-file "/etc/autossh/id_rsa_oracle")
                                                 (strict-host-key-checking? #f)
                                                 (user "opc")
                                                 (user-known-hosts-file "/dev/null")
                                                 (extra-options
                                                  "
LocalForward 0.0.0.0:8060 127.0.0.1:8060
LocalForward 0.0.0.0:6667 127.0.0.1:6667
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                                 (host "znc.wugi.info")))

                       transmission-service

                       intel-vaapi-service

                       (service webssh-service-type
                                (webssh-configuration (address "127.0.0.1")
                                                      (port 8888)
                                                      (policy 'reject)
                                                      (known-hosts '("\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOnaDeOzwmrcrq1D8slYaeFozXZ0cpqNU0EvGmgnO29aiKkSD1ehbIV4vSxk3IDXz9ClMVPc1bTUTrYhEVHdCks="
                                                                     "\
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOnaDeOzwmrcrq1D8slYaeFozXZ0cpqNU0EvGmgnO29aiKkSD1ehbIV4vSxk3IDXz9ClMVPc1bTUTrYhEVHdCks="))))

                       (modify-services (operating-system-user-services base-system)
                         (guix-service-type config => %guix-daemon-config))))

      (setuid-programs %my-setuid-programs)

      (sudoers-file (local-file "sudoers")))))

%system-guixsd
