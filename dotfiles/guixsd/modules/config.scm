(define-module (config)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages file)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages android)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages web)
  #:use-module (gnu services base)
  #:use-module (gnu services web)
  #:use-module (gnu services monitoring)
  #:use-module (wigust packages admin)
  #:use-module (wigust packages lisp)
  #:use-module (wigust packages python)
  #:use-module (wigust packages web)
  #:use-module (packages certs)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (services jenkins)
  #:use-module (services openvpn)
  #:use-module (services web)
  #:export (%guix-daemon-config
            %guix-daemon-config-with-substitute-urls

            20-intel.conf
            %my-system-packages
            %my-setuid-programs
            %nginx-deploy-hook
            letsencrypt-certificate
            letsencrypt-key

            %mtls
            proxy
            %nginx-lua-package-path
            %nginx-lua-package-cpath
            %nginx-modules
            %nginx-lua-guix

            %vm-zabbix-agent-configuration

            %githunt-nginx-configuration
            %zabbix-nginx-configuration

            %openvpn-configuration-wugi.info
            %openvpn-configuration-majordomo.ru

            %homer-nginx-configuration
            %homer-config

            %homer-wugi.info-config
            %homer-wugi.info-nginx-configuration

            %jenkins-config))

(define %guix-daemon-config
  (guix-configuration
   (authorized-keys (append (list (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/guix.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm1.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm2.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm3.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/jenkins.intr.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/spb.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/mirror.brielmaier.net.pub"))
                            %default-authorized-guix-keys))))

(define %guix-daemon-config-with-substitute-urls
  (guix-configuration
   (inherit %guix-daemon-config)
   (substitute-urls '("https://ci.guix.gnu.org"
                      "https://guix.wugi.info"
                      "https://mirror.brielmaier.net"))))

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

(define %my-system-packages
  (cons* sbcl

         stumpwm-checkout `(,stumpwm-checkout "lib")
         sbcl-stumpwm-checkout-ttf-fonts
         sbcl-stumpwm-checkout-globalwindows
         sbcl-stumpwm-checkout-swm-gaps
         sbcl-stumpwm-checkout-stumptray
         sbcl-slime-swank
         stumpish

         ;; stumpwm-next `(,stumpwm-next "lib")
         ;; sbcl-stumpwm-next-ttf-fonts
         ;; sbcl-stumpwm-next-globalwindows
         ;; sbcl-stumpwm-next-swm-gaps
         ;; sbcl-stumpwm-next-stumptray
         ;; sbcl-slime-swank
         ;; stumpish

         ncurses

         fontconfig font-awesome font-dejavu font-liberation
         font-misc-misc font-wqy-zenhei
         font-google-noto ;emoji in chromium

         gnome-themes-standard adwaita-icon-theme hicolor-icon-theme
         lxappearance

         desktop-file-utils xrdb xset xsetroot xkill
         ;; gvfs depends on webkitgtk

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

         binutils

         iptables bridge-utils

         docker-cli ;; docker-compose

         singularity

         cryptsetup

         pulseaudio

         libcgroup

         openssh ;autofs
         sshfs ;autofs
         fuse ;mount -t fuse and autofs

         file
         iftop
         net-tools
         tcpdump
         ipset
         mtr

         %base-packages))

(define %my-setuid-programs
  (cons* (file-append fping "/sbin/fping")
         (file-append ubridge "/bin/ubridge")
         (file-append iputils "/bin/ping")
         (delete (file-append inetutils "/bin/ping6")
                 (delete (file-append inetutils "/bin/ping")
                         %setuid-programs))))

(define letsencrypt-certificate
  (cut string-append "/etc/letsencrypt/live/" <> "/fullchain.pem"))

(define letsencrypt-key
  (cut string-append "/etc/letsencrypt/live/" <> "/privkey.pem"))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))


;;;
;;; NGINX
;;;

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
                (mtls? #f)
                (locations '())
                (proxy-set-header-host #f)
                (listen "192.168.0.144"))
  (nginx-server-configuration
   (server-name (if sub-domains?
                    (list (string-append sub-domains?
                                         (string-join (string-split host #\.)
                                                      "\\.")
                                         "$"))
                    (list host (string-append "www." host))))
   (locations (delete #f
                      (append locations
                              (list (nginx-location-configuration
                                     (uri "/")
                                     (body (list "resolver 80.80.80.80;"
                                                 (string-append "set $target "
                                                                (or target "127.0.0.1")
                                                                ":" (number->string port) ";")
                                                 (format #f "proxy_pass ~a://$target;" (if ssl-target? "https" "http"))
                                                 (format #f "proxy_set_header Host ~a;"
                                                         (cond (sub-domains? "$http_host;")
                                                               (proxy-set-header-host proxy-set-header-host)
                                                               (else host)))
                                                 "proxy_set_header X-Forwarded-Proto $scheme;"
                                                 "proxy_set_header X-Real-IP $remote_addr;"
                                                 "proxy_set_header X-Forwarded-for $remote_addr;"
                                                 "proxy_connect_timeout 300;"
                                                 "client_max_body_size 0;")))
                                    (and well-known?
                                         (nginx-location-configuration
                                          (uri "/.well-known")
                                          (body '("root /var/www;"))))))))
   (listen (if ssl?
               (list (string-append listen ":443 ssl"))
               (list (string-append listen ":80"))))
   (ssl-certificate (if ssl-key? (letsencrypt-certificate host) #f))
   (ssl-certificate-key (if ssl-key? (letsencrypt-key host) #f))
   (raw-content (if mtls? %mtls '()))))

(define %nginx-lua-package-path
  (list lua-resty-core
        lua-resty-lrucache
        lua-resty-signal
        lua-tablepool
        lua-resty-shell))

(define %nginx-lua-package-cpath
  (list lua-resty-signal))

(define %nginx-modules
  (list
   (file-append nginx-lua-module "/etc/nginx/modules/ngx_http_lua_module.so")))

(define %nginx-lua-guix
  (list (nginx-location-configuration
         (uri "/guix/describe")
         (body (list #~(format #f "content_by_lua_file ~s;"
                               #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/nginx/guix.lua")))))
        (nginx-location-configuration
         (uri "/git/list")
         (body (list #~(format #f "content_by_lua_file ~s;"
                               #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/nginx/mjru.lua")))))))

(define %vm-zabbix-agent-configuration
  (zabbix-agent-configuration
   (server '("zabbix.wugi.info"))
   (server-active '("zabbix.wugi.info"))
   (extra-options (string-join (list "UserParameter=release,/run/current-system/profile/bin/uname -a"
                                     "HostMetadataItem=release")
                               "\n"))))

(define %githunt-nginx-configuration
  (nginx-server-configuration
   (server-name '("githunt.wugi.info"))
   (listen '("80" "443 ssl"))
   (root (file-append (load "/home/oleg/archive/src/githunt/guix.scm") "/share/githunt"))
   (locations
    (list (nginx-location-configuration
           (uri "/.well-known")
           (body '("root /var/www;")))))
   (ssl-certificate (letsencrypt-certificate "githunt.wugi.info"))
   (ssl-certificate-key (letsencrypt-key "githunt.wugi.info"))
   (raw-content %mtls)))

(define %homer-nginx-configuration
  (nginx-server-configuration
   (inherit %homer-nginx-configuration-nginx)
   (server-name '("homer.wugi.info"))
   (listen '("80" "443 ssl"))
   (locations
    (list (nginx-location-configuration
           (uri "/.well-known")
           (body '("root /var/www;")))))
   (ssl-certificate (letsencrypt-certificate "homer.wugi.info"))
   (ssl-certificate-key (letsencrypt-key "homer.wugi.info"))
   (raw-content %mtls)))

(define %homer-wugi.info-nginx-configuration
  (nginx-server-configuration
   (inherit %homer-nginx-configuration-nginx)
   (server-name '("wugi.info"))
   (listen '("80 default_server"
             "443 ssl default_server"))
   (locations
    (list (nginx-location-configuration
           (uri "/.well-known")
           (body '("root /var/www;")))
          (nginx-location-configuration
           (uri "/assets/config.yml")
           (body '("etag off;"
                   "if_modified_since off;"
                   ;; "add_header Last-Modified $date_gmt;"
                   "add_header Last-Modified \"\";")))))
   (ssl-certificate (letsencrypt-certificate "wugi.info"))
   (ssl-certificate-key (letsencrypt-key "wugi.info"))))

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
    (listen '("80" "443 ssl"))
    (ssl-certificate (letsencrypt-certificate "zabbix.wugi.info"))
    (ssl-certificate-key (letsencrypt-key "zabbix.wugi.info"))
    (raw-content %mtls))))

(define %openvpn-configuration-wugi.info
  (openvpn-configuration
   (name "wugi.info")
   (config (plain-file "openvpn.conf"
                       "\
mode server
proto udp
port 1195
dev tapvpn1
ca /etc/openvpn/ca.crt
cert /etc/openvpn/client.crt
key /etc/openvpn/client.key
comp-lzo
persist-key
persist-tun
verb 3
ping 5
tls-server
dh /etc/openvpn/dhparams.pem
ping-restart 10
resolv-retry infinite
server 192.168.25.0 255.255.255.0
client-config-dir /etc/openvpn/ccd
client-to-client
"))))

(define %openvpn-configuration-majordomo.ru
  (openvpn-configuration
   (name "majordomo.ru")
   (config "/etc/openvpn/openvpn.conf")))


;;;
;;; Homer
;;;

(define %homer-config
  (let ((config
         '(("title" . "home.wugi.info dashboard")
           ("theme" . "default")
           ("subtitle" . "web services")
           ("services"
            .
            #((("name" . "Local")
               ("items"
                .
                #((("url" . "https://cgit.duckdns.org/")
                   ("tag" . "git")
                   ("subtitle" . "Git front-end")
                   ("name" . "Cgit")
                   ("logo" . "https://cgit.duckdns.org/share/cgit/cgit.png"))
                  (("url" . "https://githunt.wugi.info/")
                   ("tag" . "projects")
                   ("subtitle"
                    .
                    "Hunt the most starred projects on GitHub")
                   ("name" . "GitHunt")
                   ("logo" . "https://camo.githubusercontent.com/b7243ef0327a743e1c6081bddf604f421b73810d/68747470733a2f2f7261772e6769746875622e636f6d2f6b616d72616e61686d656473652f67697468756e742f6d61737465722f7075626c69632f696d672f6c6f676f2e7376673f73616e6974697a653d74727565"))
                  (("url" . "https://guix.wugi.info/")
                   ("tag" . "guix")
                   ("subtitle" . "guix publish")
                   ("name" . "Guix Substitute Server")
                   ("logo" . "https://guix.gnu.org/static/base/img/icon.png"))
                  (("url" . "http://gitlab.wugi.info/wigust")
                   ("tag" . "version-control")
                   ("subtitle" . "Source code")
                   ("name" . "GitLab")
                   ("logo" . "https://about.gitlab.com/images/press/logo/svg/gitlab-icon-rgb.svg")
                   ("type" . "Ping"))
                  (("url" . "https://home-s2x8742.slack.com/")
                   ("tag" . "chat")
                   ("subtitle" . "Chat")
                   ("name" . "Slack")
                   ("logo" . "https://upload.wikimedia.org/wikipedia/commons/b/b9/Slack_Technologies_Logo.svg"))
                  (("url" . "https://syncthing.wugi.info/")
                   ("tag" . "syncthing")
                   ("subtitle" . "Syncthing")
                   ("name" . "Syncthing")
                   ("logo" . "https://upload.wikimedia.org/wikipedia/commons/a/a2/SyncthingLogoHorizontal.svg"))
                  (("url" . "https://torrent.wugi.info/")
                   ("tag" . "torrent")
                   ("subtitle" . "Torrent client")
                   ("name" . "Transmission")
                   ("logo" . "https://upload.wikimedia.org/wikipedia/commons/4/46/Transmission_Icon.svg"))
                  (("url" . "http://localhost:8060/")
                   ("tag" . "irc")
                   ("subtitle" . "IRC gateway")
                   ("name" . "ZNC")
                   ("logo" . "https://kotabatu.net/wp-content/uploads/2012/04/spoof-identd-znc.svg"))))
               ("icon" . "fas fa-home"))
              (("name" . "Admin")
               ("items"
                .
                #((("url" . "https://jenkins.wugi.info/")
                   ("tag" . "devops")
                   ("subtitle" . "CI/CD")
                   ("name" . "Jenkins")
                   ("logo" . "https://upload.wikimedia.org/wikipedia/commons/e/e3/Jenkins_logo_with_title.svg"))
                  (("url" . "http://localhost:9090/")
                   ("tag" . "prometheus")
                   ("subtitle" . "Prometheus")
                   ("name" . "Prometheus")
                   ("type" . "Prometheus")
                   ("logo" . "https://raw.githubusercontent.com/prometheus/docs/ca2961b495c3e2a1e4586899c26de692fa5a28e7/static/prometheus_logo_orange_circle.svg"))
                  (("url" . "http://127.0.0.1:18578")
                   ("tag" . "prometheus")
                   ("subtitle" . "Alertmanager front-end")
                   ("name" . "Karma")
                   ("logo" . "https://raw.githubusercontent.com/prometheus/docs/ca2961b495c3e2a1e4586899c26de692fa5a28e7/static/prometheus_logo_orange_circle.svg"))
                  (("url" . "http://127.0.0.1:5601/app/login?nextUrl=%2F")
                   ("tag" . "opensearch")
                   ("subtitle" . "OpenSearch front-end")
                   ("name" . "OpenSearch")
                   ("logo" . "https://upload.wikimedia.org/wikipedia/commons/f/f4/Elasticsearch_logo.svg"))
                  (("url" . "http://127.0.0.1:3000")
                   ("tag" . "grafana")
                   ("subtitle" . "Dashboards")
                   ("name" . "Grafana")
                   ("logo" . "https://upload.wikimedia.org/wikipedia/en/a/a1/Grafana_logo.svg"))
                  (("url" . "https://webssh.wugi.info/")
                   ("tag" . "ssh")
                   ("subtitle" . "SSH web-client")
                   ("name" . "WebSSH")
                   ("logo" . "https://webssh.wugi.info/static/img/favicon.png")
                   ("type" . "Ping"))))
               ("icon" . "fas fa-home"))
              (("name" . "Third-Party hosting")
               ("items"
                .
                #((("url" . "https://wugi.info/")
                   ("tag" . "blog")
                   ("subtitle" . "My blog")
                   ("name" . "Blog")
                   ("logo" . "https://d29fhpw069ctt2.cloudfront.net/icon/image/84747/preview.svg"))
                  (("url" . "https://192.168.125.14/ui/#/login")
                   ("tag" . "vps")
                   ("subtitle" . "Guix machine on VMware")
                   ("name" . "VMware ESXI")
                   ("logo" . "https://www.leaseweb.com/sites/default/files/Images/09_Products/vmware-vsphere.svg"))
                  (("url" . "https://console.eu-frankfurt-1.oraclecloud.com/?tenant=wigust&provider=OracleIdentityCloudService")
                   ("tag" . "vps")
                   ("subtitle" . "Oracle Linux machines")
                   ("name" . "Oracle Cloud")
                   ("logo" . "https://www.oracle.com/ocom/groups/public/@otn/documents/digitalasset/5303705.png"))
                  (("url" . "https://dcc.godaddy.com/manage/dns?domainName=wugi.info")
                   ("tag" . "dns")
                   ("subtitle" . "wugi.info zone")
                   ("name" . "DNS")
                   ("logo" . "https://upload.wikimedia.org/wikipedia/commons/e/ef/Oxygen480-categories-applications-internet.svg"))))
               ("icon" . "fas fa-cloud"))))
           ("logo" . "logo.png")
           ("links"
            .
            #((("url" . "https://github.com/bastienwirtz/homer")
               ("target" . "_blank")
               ("name" . "Contribute")
               ("icon" . "fab fa-github"))
              (("url" . "https://www.wikipedia.org/")
               ("name" . "Wiki")
               ("icon" . "fas fa-book"))))
           ("header" . #t)
           ("footer" . "<p>Created with <span class=\"has-text-danger\">❤️</span> with <a href=\"https://bulma.io/\">bulma</a>, <a href=\"https://vuejs.org/\">vuejs</a> & <a href=\"https://fontawesome.com/\">font awesome</a> // Fork me on <a href=\"https://github.com/bastienwirtz/homer\"><i class=\"fab fa-github-alt\"></i></a></p>")
           ("colors"
            ("light"
             ("text-title" . "#303030")
             ("text-subtitle" . "#424242")
             ("text-header" . "#ffffff")
             ("text" . "#363636")
             ("link-hover" . "#363636")
             ("highlight-secondary" . "#4285f4")
             ("highlight-primary" . "#3367d6")
             ("highlight-hover" . "#5a95f5")
             ("card-shadow" . "rgba(0, 0, 0, 0.1)")
             ("card-background" . "#ffffff")
             ("background" . "#f5f5f5"))
            ("dark"
             ("text-title" . "#fafafa")
             ("text-subtitle" . "#f5f5f5")
             ("text-header" . "#ffffff")
             ("text" . "#eaeaea")
             ("link-hover" . "#ffdd57")
             ("highlight-secondary" . "#4285f4")
             ("highlight-primary" . "#3367d6")
             ("highlight-hover" . "#5a95f5")
             ("card-shadow" . "rgba(0, 0, 0, 0.4)")
             ("card-background" . "#2b2b2b")
             ("background" . "#131313"))))))
    (computed-file
     "config.json"
     (with-extensions (list guile-json-4)
       (with-imported-modules (source-module-closure '((json builder)))
         #~(begin
             (use-modules (json builder))
             (with-output-to-file #$output
               (lambda ()
                 (display (scm->json '#$config #:pretty #t))))))))))

(define %homer-wugi.info-config
  (let ((config
         `(("title" . "Home page")
           ("subtitle" . "wugi.info")
           ("message"
            ("style" . "is-warning")
            ("title" . "Hello!")
            ("icon" . "fas fa-hand-paper")
            ("content" . "I'm Oleg Pykhalov, also known as WiGust. This is my homepage."))
           ("theme" . "default")
           ("services" . #((("name" . "Writing")
                            ("items" . #((("url" . "https://blog.wugi.info/")
                                          ("tag" . "web")
                                          ("subtitle" . "Articles")
                                          ("name" . "Blog")
                                          ("logo" . "https://upload.wikimedia.org/wikipedia/commons/0/08/EmacsIcon.svg"))))
                            ("icon" . "fas fa-pen-nib"))
                           (("name" . "Programming")
                            ("items" . #((("url" . "https://gitlab.com/wigust/")
                                          ("tag" . "version-control")
                                          ("subtitle" . "Source code")
                                          ("name" . "GitLab")
                                          ("logo" . "https://about.gitlab.com/images/press/logo/svg/gitlab-icon-rgb.svg"))))
                            ("icon" . "fas fa-file-code"))
                           (("name" . "Networking")
                            ("items" . #((("url" . "https://dn42.eu/")
                                          ("tag" . "networking")
                                          ("name" . "dn42")
                                          ("subtitle" . "AS4242422496")
                                          ("logo" . "https://dn42.eu/dn42.svg"))))
                            ("icon" . "fas fa-network-wired"))
                           (("name" . "Contact me")
                            ("items" . #((("url" . "mailto:go.wigust@gmail.com")
                                          ("tag" . "mail")
                                          ("name" . "Mail")
                                          ("subtitle" . "go.wigust@gmail.com")
                                          ;; ("icon" . "fab fa-envelope")
                                          ("logo" . "https://upload.wikimedia.org/wikipedia/commons/7/7e/Gmail_icon_%282020%29.svg"))
                                         (("url" . "https://matrix.to/#/@wigust:matrix.org")
                                          ("tag" . "messaging")
                                          ("name" . "Matrix")
                                          ("subtitle" . "@wigust:matrix.org")
                                          ("logo" . "https://element.io/images/logo-mark-primary.svg"))
                                         (("url" . "https://mastodon.xyz/@wigust")
                                          ("tag" . "messaging")
                                          ("name" . "Mastodon")
                                          ("subtitle" . "mastodon.xyz/@wigust")
                                          ("logo" . "https://upload.wikimedia.org/wikipedia/commons/4/48/Mastodon_Logotype_%28Simple%29.svg"))))
                            ("icon" . "fas fa-user-circle"))))
           ("logo" . "logo.png")
           ;; ("links" . #((("url" . "mailto:go.wigust@gmail.com")
           ;;               ("name" . "Email")
           ;;               ("icon" . "fab fa-envelope"))))
           ("header" . #t)
           ("footer" . "<p>Powered by <a href=\"https://github.com/bastienwirtz/homer\">Homer</a>.")
           ("colors"
            ("light"
             ("text-title" . "#303030")
             ("text-subtitle" . "#424242")
             ("text-header" . "#ffffff")
             ("text" . "#363636")
             ("link-hover" . "#363636")
             ("highlight-secondary" . "#4285f4")
             ("highlight-primary" . "#3367d6")
             ("highlight-hover" . "#5a95f5")
             ("card-shadow" . "rgba(0, 0, 0, 0.1)")
             ("card-background" . "#ffffff")
             ("background" . "#f5f5f5"))
            ("dark"
             ("text-title" . "#fafafa")
             ("text-subtitle" . "#f5f5f5")
             ("text-header" . "#ffffff")
             ("text" . "#eaeaea")
             ("link-hover" . "#ffdd57")
             ("highlight-secondary" . "#4285f4")
             ("highlight-primary" . "#3367d6")
             ("highlight-hover" . "#5a95f5")
             ("card-shadow" . "rgba(0, 0, 0, 0.4)")
             ("card-background" . "#2b2b2b")
             ("background" . "#131313"))))))
    (computed-file
     "config.json"
     (with-extensions (list guile-json-4)
       (with-imported-modules (source-module-closure '((json builder)))
         #~(begin
             (use-modules (json builder))
             (with-output-to-file #$output
               (lambda ()
                 (scm->json '#$config)))))))))


;;;
;;; Jenkins
;;;

(define %jenkins-config
  (jenkins-configuration
   (java "/home/oleg/.nix-profile")
   (plugins (load "/home/oleg/.local/share/chezmoi/dotfiles/jenkins/plugins.scm"))
   (arguments
    '("--httpListenAddress=127.0.0.1"
      "--httpPort=8090"
      "--ajp13Port=-1"))
   (java-arguments
    '(;; "-Djava.awt.headless=true"
      "-Xmx512m"
      "-Djenkins.install.runSetupWizard=false"

      ;; "-Djavax.net.debug=all"
      "-Djavax.net.ssl.trustStore=/home/oleg/.nix-profile/lib/openjdk/lib/security/cacerts"

      ;; "-Dorg.jenkinsci.plugins.durabletask.BourneShellScript.LAUNCH_DIAGNOSTICS=true"
      ;; "-Dorg.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL=86400"

      ;; https://www.jenkins.io/doc/book/security/configuring-content-security-policy/
      "-Dhudson.model.DirectoryBrowserSupport.CSP="

      ;; Managing Security
      ;; <https://www.jenkins.io/doc/book/managing/security/#disable-csrf-checking>
      ;;
      ;; Upgrading to Jenkins LTS 2.176.x
      ;; <https://www.jenkins.io/doc/upgrade-guide/2.176/#SECURITY-626>
      ;;
      ;; Upgrading to Jenkins LTS 2.222.x
      ;; <https://www.jenkins.io/doc/upgrade-guide/2.222/#always-enabled-csrf-protection>

      ;; Managing Security
      ;; <https://www.jenkins.io/doc/book/managing/security/#caveats>
      "-Dhudson.security.csrf.GlobalCrumbIssuerConfiguration.DISABLE_CSRF_PROTECTION=true"))
   (environment-variables
    (append (list #~(string-append "CASC_JENKINS_CONFIG="
                                   #$(computed-file
                                      "jenkins.json"
                                      (with-extensions (list guile-json-4)
                                        (with-imported-modules (source-module-closure '((json builder)))
                                          #~(begin
                                              (use-modules (json builder))
                                              (define jenkins-jobs
                                                #$(local-file "../../jenkins/jobs.groovy"))
                                              (with-output-to-file #$output
                                                (lambda ()
                                                  (scm->json
                                                   `(("credentials"
                                                      ("system"
                                                       ("domainCredentials"
                                                        .
                                                        #((("credentials"
                                                            .
                                                            #((("basicSSHUserPrivateKey"
                                                                ("username" . "jenkins")
                                                                ("scope" . "GLOBAL")
                                                                ("privateKeySource"
                                                                 ("directEntry"
                                                                  ("privateKey"
                                                                   .
                                                                   "${readFile:/var/lib/jenkins/.ssh/id_rsa}")))
                                                                ("id" . "jenkins-ssh-deploy")
                                                                ("description" . "jenkins-ssh-deploy"))))))))))
                                                     ("unclassified"
                                                      ("pollSCM"
                                                       ("pollingThreadCount" . 10))
                                                      ("location"
                                                       ("url" . "https://jenkins.wugi.info/")
                                                       ("adminAddress" . "admin@wugi.info"))
                                                      ("globalLibraries"
                                                       ("libraries" .
                                                        #((("retriever"
                                                            ("modernSCM"
                                                             ("scm"
                                                              ("git"
                                                               ("traits" .
                                                                #("gitBranchDiscovery"))
                                                               ("remote" . "https://gitlab.com/wigust/jenkins-shared-library")))))
                                                           ("name" . "jenkins-wi-shared-library")
                                                           ("implicit" . #t)
                                                           ("defaultVersion" . "master")))))
                                                      ("defaultFolderConfiguration"
                                                       ("healthMetrics" .
                                                        #((("worstChildHealthMetric"
                                                            ("recursive" . #t)))))))
                                                     ("security"
                                                      ("sSHD"
                                                       ("port" . -1))
                                                      ("globalJobDslSecurityConfiguration"
                                                       ("useScriptSecurity" . #t))
                                                      ("apiToken"
                                                       ("usageStatisticsEnabled" . #t)
                                                       ("tokenGenerationOnCreationEnabled" . #f)
                                                       ("creationOfLegacyTokenEnabled" . #f)))
                                                     ("jenkins"
                                                      ("nodes" .
                                                       #((("permanent"
                                                           ("retentionStrategy"
                                                            ("demand"
                                                             ("inDemandDelay" . 0)
                                                             ("idleDelay" . 30)))
                                                           ("remoteFS" . "/var/lib/jenkins")
                                                           ("numExecutors" . 15)
                                                           ("nodeDescription" . "Guix system")
                                                           ("name" . "guixsd")
                                                           ("launcher"
                                                            ("ssh"
                                                             ("sshHostKeyVerificationStrategy" . "nonVerifyingKeyVerificationStrategy")
                                                             ("port" . 22)
                                                             ("javaPath" . "/home/oleg/.nix-profile/bin/java")
                                                             ("host" . "localhost")
                                                             ("credentialsId" . "jenkins-ssh-deploy")))))))
                                                      ("securityRealm"
                                                       ("local"
                                                        ("users" . #((("id" . "${DATA_VAULTPASS_WUGI.INFO_JENKINS.WUGI.INFO_USERNAME}")
                                                                      ("password" . "${DATA_VAULTPASS_WUGI.INFO_JENKINS.WUGI.INFO_PASSWORD}"))))
                                                        ("allowsSignup" . #f))))
                                                     ("jobs" . #((("file" . ,jenkins-jobs)))))
                                                   #:pretty #t))))))))
                  "SECRETS_FILE=/etc/jenkins/jenkins.properties")
            (string-split
             (string-delete #\"
                            (string-trim-right
                             (with-input-from-file "/etc/jenkins/jenkins.properties"
                               (@ (ice-9 rdelim) read-string))))
             #\newline)))))
