(use-modules (gnu) (srfi srfi-1) (srfi srfi-26))
(use-modules (services docker))
(use-package-modules admin base certs lisp suckless xdisorg xorg fonts
                     android fontutils gnome freedesktop readline networking)
(use-service-modules admin dbus desktop dns networking sound xorg ssh
                     web certbot monitoring databases mail)

(define 20-intel.conf "\
# This block fixes tearing on Intel GPU.
# Origin: https://wiki.archlinux.org/index.php/Intel_Graphics
#         https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"AccelMethod\"  \"sna\"
   Option      \"SwapbuffersWait\" \"true\"
   Option      \"TearFree\" \"true\"
EndSection\n")


;;;
;;; Certbot
;;;

(define letsencrypt-certificate
  (cut string-append "/etc/letsencrypt/live/" <> "/fullchain.pem"))

(define letsencrypt-key
  (cut string-append "/etc/letsencrypt/live/" <> "/privkey.pem"))


;;;
;;; NGINX
;;;

(define %nginx-certbot
  (nginx-location-configuration
   (uri "/.well-known")
   (body '("root /var/www;"))))

(define* (proxy host port #:key (ssl? #f) (ssl-target? #f)
                (well-known? #t) (target #f))
  (nginx-server-configuration
   (server-name (list host (string-append "www." host)))
   (locations (delete #f
                      (list (nginx-location-configuration
                             (uri "/")
                             (body (list "resolver 80.80.80.80;"
                                         (string-append "set $target "
                                                        (or target "localhost")
                                                        ":" (number->string port) ";")
                                         (format #f "proxy_pass ~a://$target;" (if ssl-target? "https" "http"))
                                         (format #f "proxy_set_header Host ~a;" host)
                                         "proxy_set_header X-Forwarded-Proto $scheme;"
                                         "proxy_set_header X-Real-IP $remote_addr;"
                                         "proxy_set_header X-Forwarded-for $remote_addr;"
                                         "proxy_connect_timeout 300;")))
                            (and well-known?
                                 (nginx-location-configuration
                                  (uri "/.well-known")
                                  (body '("root /var/www;")))))))
   (listen (if ssl?
               (list "443 ssl")
               (list "80")))
   (ssl-certificate (if ssl?
                        (letsencrypt-certificate host)
                        #f))
   (ssl-certificate-key (if ssl?
                            (letsencrypt-key host)
                            #f))))

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
        (proxy "cups.tld" 631)
        (proxy "torrent.tld" 9091)
        (proxy "awx.wugi.info" 80 #:target "192.168.105.133" #:ssl? #t)
        (proxy "jenkins.wugi.info" 30080 #:ssl? #t)
        (proxy "alerta.wugi.info" 47080 #:ssl? #t)
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
        (proxy "guix.duckdns.org" 5556 #:ssl? #t)))

(define %zabbix-nginx-configuration
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
    (ssl-certificate-key (letsencrypt-key "zabbix.wugi.info")))))


;;;
;;; Entryp point
;;;

(define %system-guixsd
  (let ((base-system (load "/etc/config.scm")))
    (operating-system
      (inherit base-system)
      (kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"))
      (packages (cons* stumpwm sbcl-slime-swank `(,stumpwm "lib")

                       fontconfig font-awesome font-dejavu font-liberation
                       font-misc-misc font-wqy-zenhei

                       adwaita-icon-theme hicolor-icon-theme

                       desktop-file-utils gvfs xrdb xset xsetroot xkill

                       setxkbmap   ;keyboard layout
                       wmctrl      ;`ewmctrl'
                       xclip       ;X clipboard CLI
                       xdg-utils   ;finds a program to open file
                       xdotool     ;mouse and keyboard automation
                       xorg-server ;`xephyr' for x11 testing
                       xrandr      ;change screen resolution
                       xterm       ;$TERM terminal
                       xwininfo    ;X window information
                       ;; For helm-stumpwm-commands and stumpish
                       rlwrap
                       xprop
                       xhost

                       nss-certs ;SSL certificates

                       fping

                       adb

                       (operating-system-packages base-system)))

      (groups (cons* (user-group (name "nixbld")
                                 (id 30100))
                     ;; (user-group (name "adbusers"))
                     ;; (user-group (name "guix-offload"))
                     ;; (user-group (name "telegraf") (system? #t))
                     ;; (user-group (name "git") (id 30003))
                     ;; (user-group (name "jenkins") (id 30004))
                     ;; (user-group (name "influxdb") (id 30005))
                     ;; (user-group (name "grafana") (id 30006))
                     (user-group (name "adbusers"))
                     (user-group (name "docker")
                                 (system? #t))
                     ;; (user-group (name "jenkinsbuild"))
                     ;; (user-group (name "alerta"))
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
        (string-append
         (local-host-aliases (operating-system-host-name base-system))
         "\n\n"
         "192.168.100.1 r1.tld\n"
         "192.168.105.1 r2.tld\n"
         "192.168.105.120 cuirass.tld\n"
         "127.0.0.1 gitlab.wugi.info grafana.wugi.info zabbix.wugi.info"
         "\n\n"
         %facebook-host-aliases)))

      (services (cons* (extra-special-file "/usr/bin/env"
                                           (file-append coreutils "/bin/env"))

                       ;; “adb” and “fastboot” without root privileges
                       (simple-service 'adb udev-service-type (list android-udev-rules))

                       ;; Desktop services
                       (service slim-service-type
                                (slim-configuration
                                 (xorg-configuration
                                  (xorg-configuration
                                   (extra-config (list 20-intel.conf))))))
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

                       docker-service

                       (service openssh-service-type
                                (openssh-configuration
                                 (x11-forwarding? #t)
                                 (gateway-ports? 'client)
                                 (password-authentication? #f)))

                       (service php-fpm-service-type
                                (php-fpm-configuration
                                 (timezone "Europe/Moscow")))

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
                                                 "prometheus.wugi.info"
                                                 "alerta.wugi.info"
                                                 "awx.wugi.info"))))))

                       (service nginx-service-type
                                (nginx-configuration
                                 (server-blocks %nginx-server-blocks)))

                       ;; (tor-service (string-append (dirname (current-filename))
                       ;;                             "/torrc"))

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

                       (postgresql-service #:config-file (postgresql-config-file
                                                          (hba-file
                                                           (plain-file "pg_hba.conf"
                                                                       "
local	all	all			trust
host	all	all	127.0.0.1/32    trust
host	all	all	::1/128         trust
host	all	all	172.16.0.0/12   trust"))
                                                          (extra-config '(("listen_addresses" "'0.0.0.0'")))))

                       (service zabbix-server-service-type
                                (zabbix-server-configuration
                                 (include-files '("/etc/zabbix/zabbix-server.secret"))
                                 (extra-options "
AlertScriptsPath=/etc/zabbix/alertscripts
ExternalScripts=/etc/zabbix/externalscripts
FpingLocation=/run/setuid-programs/fping
")))

                       (service zabbix-agent-service-type)

                       (service zabbix-front-end-service-type
                                (zabbix-front-end-configuration
                                 (db-secret-file "/etc/zabbix/zabbix.secret")
                                 (nginx %zabbix-nginx-configuration)))

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
                                 (host "0.0.0.0")
                                 (port 5556)))

                       (operating-system-user-services base-system)))

      (setuid-programs (cons* (file-append fping "/sbin/fping")
                              (file-append ubridge "/bin/ubridge")
                              %setuid-programs)))))

%system-guixsd
