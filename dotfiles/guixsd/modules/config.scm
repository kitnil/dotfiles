(define-module (config)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages file)
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
  #:use-module (packages majordomo)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (services openvpn)
  #:export (%guix-daemon-config

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
            %homer-nginx-configuration
            %zabbix-nginx-configuration

            %openvpn-configuration-wugi.info
            %openvpn-configuration-majordomo.ru))

(define %guix-daemon-config
  (guix-configuration
   (authorized-keys (append (list (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/guix.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm1.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm2.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/vm3.wugi.info.pub")
                                  (local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/substitutes/spb.pub"))
                            %default-authorized-guix-keys))))


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
  (cons* sbcl stumpwm-checkout `(,stumpwm-checkout "lib")

         sbcl-stumpwm-checkout-ttf-fonts
         sbcl-stumpwm-checkout-globalwindows
         sbcl-stumpwm-checkout-swm-gaps
         sbcl-stumpwm-checkout-stumptray
         sbcl-slime-swank
         stumpish

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

         iptables bridge-utils

         docker-cli docker-compose

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
                (proxy-set-header-host #f))
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
               (list "443 ssl")
               (list "80")))
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
   (listen '("443 ssl"))
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
   (server-name '("homer.wugi.info"))
   (listen '("443 ssl"))
   (root (file-append homer "/share/homer"))
   (locations
    (list (nginx-location-configuration
           (uri "/.well-known")
           (body '("root /var/www;")))))
   (ssl-certificate (letsencrypt-certificate "homer.wugi.info"))
   (ssl-certificate-key (letsencrypt-key "homer.wugi.info"))
   (raw-content %mtls)))

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

(define %openvpn-configuration-wugi.info
  (openvpn-configuration
   (name "wugi.info")
   (config (plain-file "openvpn.conf"
                       "\
client
proto udp
dev tun
ca /etc/openvpn/ca.crt
cert /etc/openvpn/client.crt
key /etc/openvpn/client.key
comp-lzo
persist-key
persist-tun
verb 3
nobind
ping 5
ping-restart 10
resolv-retry infinite
remote vm1.wugi.info 1195
remote vm2.wugi.info 1194
remote-random
"))))

(define %openvpn-configuration-majordomo.ru
  (openvpn-configuration
   (name "majordomo.ru")
   (config "/etc/openvpn/openvpn.conf")))
