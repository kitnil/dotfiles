(use-modules (gnu)
             (srfi srfi-1)
             (srfi srfi-26))

(use-package-modules admin base certs cryptsetup docker linux lisp
                     suckless xdisorg xorg fonts android fontutils
                     gnome freedesktop readline ncurses networking
                     pulseaudio)

(use-service-modules admin dbus desktop docker dns networking sound
                     xorg ssh web cgit version-control certbot
                     monitoring databases mail)

;; Third-party modules
(use-modules (wigust services nix)
             (wigust services autossh)
             (wigust services kresd)
             (wigust services openvpn)
             (wigust packages lisp)
             (wigust packages php)
             (majordomo packages majordomo))

(define 20-intel.conf "\
# Fix tearing for Intel graphics card.
# Origin: https://wiki.archlinux.org/index.php/Intel_Graphics
#         https://github.com/8p8c/my-guix/blob/master/config.scm
Section \"Device\"
   Identifier  \"Intel Graphics\"
   Driver      \"intel\"
   Option      \"AccelMethod\"  \"sna\"
   Option      \"SwapbuffersWait\" \"true\"
   Option      \"TearFree\" \"true\"
EndSection\n")

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
        "guix.duckdns.org"
        "alerta.duckdns.org"
        "anongit.duckdns.org"
        "pipeline.duckdns.org"
        ;; TODO: "wugi.info"
        "zabbix.wugi.info"
        "grafana.wugi.info"
        "jenkins.wugi.info"
        "gitlab.wugi.info"
        "gitea.wugi.info"
        "prometheus.wugi.info"
        "alerta.wugi.info"
        "awx.wugi.info"
        "stackstorm.wugi.info"
        "nextcloud.wugi.info"
        "redmine.wugi.info"))


;;;
;;; NGINX
;;;

(define %nginx-certbot
  (nginx-location-configuration
   (uri "/.well-known")
   (body '("root /var/www;"))))

(define* (proxy host port
                #:key
                (ssl? #f)
                (ssl-target? #f)
                (well-known? #t)
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
                             (uri "/")
                             (body (list "resolver 80.80.80.80;"
                                         (string-append "set $target "
                                                        (or target "localhost")
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
        (nginx-server-configuration
         (server-name '("techinfo.intr"))
         (listen '("80"))
         (root "/var/www/techinfo.intr"))
        (nginx-server-configuration
         (server-name '("texinfo.tld"))
         (listen '("80"))
         (root "/var/www/texinfo"))
        (proxy "cups.tld" 631)
        (proxy "torrent.tld" 9091)
        (proxy "awx.wugi.info" 8052 #:ssl? #t)
        (proxy "stackstorm.wugi.info" 4443 #:ssl? #t #:ssl-target? #t)
        (proxy "jenkins.wugi.info" 8090 #:ssl? #t)
        (proxy "alerta.wugi.info" 47080 #:ssl? #t)
        (proxy "grafana.wugi.info" 3080 #:ssl? #t)
        (proxy "dashboard.gitlab.wugi.info" 64680)
        (proxy "gitlab.wugi.info" 65080 #:sub-domains? "~^(?<group>.*)\\.")
        (proxy "gitlab.wugi.info" 65080  #:ssl? #t)
        (proxy "gitea.wugi.info" 3000 #:ssl? #t)
        (proxy "anongit.duckdns.org" 65080  #:ssl? #t)
        (proxy "cuirass.tld" 19080)
        (proxy "input.tld" 19080)
        (proxy "prometheus.wugi.info" 65090 #:ssl? #t)
        (proxy "alerta.intr" 16180)
        (proxy "web.alerta.intr" 16480)
        (proxy "zabbix.intr" 15081)
        (proxy "cerberus.intr" 15080)
        (proxy "grafana.intr" 16080)
        (proxy "nextcloud.wugi.info" 28080 #:ssl? #t)
        (proxy "redmine.wugi.info" 44080 #:ssl? #t)
        (proxy "guix.duckdns.org" 5556 #:ssl? #t)
        (proxy "hms.majordomo.ru" 7777 #:ssl? #f)
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

(define %slim-theme
  (or (and=> (current-filename)
             (lambda (file)
               (string-append (dirname (dirname file))
                              "/fiore/modules/slim-artwork.scm")))
      "/home/oleg/src/dotfiles/fiore/modules/slim-artwork.scm"))

(define %hardware-file
  (or (and=> (current-filename)
             (lambda (file)
               (string-append (dirname file) "/hardware/guixsd.scm")))
      "/home/oleg/src/dotfiles/guixsd/hardware/guixsd.scm"))

(define %system-guixsd
  (let ((base-system (load %hardware-file)))
    (operating-system
      (inherit base-system)
      (kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"))
      (packages (cons* sbcl stumpwm-checkout `(,stumpwm-checkout "lib")

                       ncurses

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
                       majordomo-ca

                       fping

                       adb

                       iptables bridge-utils

                       docker-cli docker-compose

                       cryptsetup

                       pulseaudio

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
                     (name "stanley")
                     (comment "Stanley StackStorm")
                     (group "users")
                     (supplementary-groups '("kvm"))
                     (home-directory "/home/stanley"))
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
                    (user-account
                     (name "awx")
                     (comment "AWX privilege separation user")
                     (group "users")
                     (supplementary-groups '("docker"))
                     (home-directory "/home/awx"))
                    (user-account
                     (name "jenkins")
                     (comment "Jenkins")
                     (group "users")
                     (supplementary-groups '("adbusers" "docker" "kvm"))
                     (home-directory "/home/jenkins"))
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
                           "alerta.wugi.info"
                           "cuirass.wugi.info"
                           "dashboard.gitlab.wugi.info"
                           "gitea.wugi.info"
                           "gitlab.wugi.info"
                           "grafana.wugi.info"
                           "zabbix.wugi.info"
                           "techinfo.intr"
                           "texinfo.tld"))
           "::1 guixsd localhost"

           "192.168.100.1 r1.tld"
           "192.168.105.1 r2.tld"
           "192.168.105.120 cuirass.tld"
           "172.16.100.60 workstation.intr"
           "178.250.247.125 gitlab.mjtest jenkins.mjtest"
           "130.61.95.6 oracle"

           ,%facebook-host-aliases)
         "\n")))

      (services (cons* (extra-special-file "/usr/bin/env"
                                           (file-append coreutils "/bin/env"))

                       ;; “adb” and “fastboot” without root privileges
                       (simple-service 'adb udev-service-type (list android-udev-rules))

                       ;; Desktop services
                       (service slim-service-type
                                (slim-configuration
				 (theme %slim-theme)
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

                       (service openssh-service-type
                                (openssh-configuration
                                 (x11-forwarding? #t)
                                 (gateway-ports? 'client)
                                 (password-authentication? #f)))

                       (service php-fpm-service-type
                                (php-fpm-configuration
                                 (php php-7.3.12)
                                 (timezone "Europe/Moscow")))

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

                       (service zabbix-agent-service-type
                                (zabbix-agent-configuration
;; UserParameter=guix.channel.list[*],/etc/zabbix/externalscripts/zabbix_guix --profile=$1 --remote=$2 --available
;; UserParameter=guix.channel.diff[*],/etc/zabbix/externalscripts/zabbix_guix --profile=$1 --remote=$2 --diff=$3
;; UserParameter=cpu.sensors,/etc/zabbix/externalscripts/zabbix_sensors
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

                       (service autossh-service-type
                                (autossh-configuration
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

                       (service docker-service-type)

                       (operating-system-user-services base-system)))

      (setuid-programs (cons* (file-append fping "/sbin/fping")
                              (file-append mtr "/sbin/mtr")
                              (file-append ubridge "/bin/ubridge")
                              %setuid-programs))

      (sudoers-file (local-file "sudoers")))))

%system-guixsd
