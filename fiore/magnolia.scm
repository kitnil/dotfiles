;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017, 2018 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(define-module (magnolia)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

(use-service-modules cups desktop dns mail networking rsync shepherd
spice ssh version-control web xorg cgit)

(use-package-modules admin android backup bash bootloaders certs cups
databases dns file fonts fontutils freedesktop gnome gnupg graphviz
linux ncurses networking ratpoison readline rsync pulseaudio screen
ssh version-control virtualization wget xdisorg xorg zile)

(define %source-dir (dirname (current-filename)))


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
       (iptables "-A INPUT -p tcp --dport 22 \
-m state --state NEW -m recent --set --name SSH -j ACCEPT")
       (iptables "-A INPUT -p tcp --dport 22 \
-m recent --update --seconds 60 --hitcount 4 --rttl \
--name SSH -j LOG --log-prefix SSH_brute_force")
       (iptables "-A INPUT -p tcp --dport 22 \
-m recent --update --seconds 60 --hitcount 4 --rttl \
--name SSH -j DROP")

       ;; TODO: Map over a list of ports
       (iptables "-A INPUT -p tcp --dport 80 -s 192.168.0.0/16 -j ACCEPT") ; web
       (iptables "-A INPUT -p tcp --dport 445 -s 192.168.0.0/16 -j ACCEPT") ; smb
       (iptables "-A INPUT -p tcp --dport 3389 -s 192.168.0.0/16 -j ACCEPT") ; rdp
       (iptables "-A INPUT -p tcp --dport 80 -s 127.0.0.0/8 -j ACCEPT")
       (iptables "-A INPUT -p tcp --dport 80 -j DROP")

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
;;; udev
;;;

(define (quote-string str)
  (string-append "\"" str "\""))

;; Origin <https://wiki.archlinux.org/index.php/Wake-on-LAN#udev>.
(define %udev-rule-won
  (udev-rule
   "99-wol.rules"
   (string-append "ACTION==" (quote-string "add")
                  ", SUBSYSTEM==" (quote-string "net")
                  ", RUN+=" (quote-string "/run/current-system/profile\
/sbin/ethtool -s $name wol g"))))


;;;
;;; guix-daemon
;;;

(define %guix-daemon-config
  (guix-configuration
   ;; Import keys
   ;; $ wget https://git.savannah.gnu.org/cgit/guix/maintenance.git/plain/hydra/keys/guix/berlin.guixsd.org-export.pub
   ;; # “guix archive --authorize < berlin.guixsd.org-export.pub”
   (substitute-urls '("https://berlin.guixsd.org"
                      "https://mirror.hydra.gnu.org" "https://hydra.gnu.org"))
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
           (root "/srv/share")
           (ssl-certificate #f)
           (ssl-certificate-key #f))))))

(define cups-nginx-service
  (simple-service 'torrent-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("print.magnolia.local"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:631;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define torrent-nginx-service
  (simple-service 'torrent-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("torrent.magnolia.local"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:9091;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define guix-publish-nginx-service
  (simple-service 'guix-publish-nginx nginx-service-type
   (list (nginx-server-configuration
          (server-name '("guix.magnolia.local"))
          (locations (list (nginx-location-configuration
                            (uri "/")
                            (body '("proxy_pass http://localhost:3000;")))))
          (ssl-certificate #f)
          (ssl-certificate-key #f)))))

(define natsu-nginx-service
  (simple-service 'natsu-nginx nginx-service-type
   (list (nginx-server-configuration
         (server-name '("natsu.magnolia.local"))
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
   (server-name '("cgit.magnolia.local"))))


;;;
;;; KNOT
;;;

(define-zone-entries local.zone
  ;; Name TTL Class Type Data
  ("@"  ""  "IN"  "A"  "127.0.0.1")
  ("@"  ""  "IN"  "NS" "ns")
  ("ns" ""  "IN"  "A"  "127.0.0.1")
  ("cgit.magnolia.local." ""  "IN"  "A"  "192.168.105.120")
  ("guix.magnolia.local." ""  "IN"  "A"  "192.168.105.120")
  ("www.magnolia.local." ""  "IN"  "A"  "192.168.105.120")
  ("print.magnolia.local." ""  "IN"  "A"  "192.168.105.120")
  ("torrent.magnolia.local." ""  "IN"  "A"  "192.168.105.120")
  ("natsu.magnolia.local." ""  "IN"  "A"  "192.168.105.120"))

(define master-zone
  (knot-zone-configuration
   (domain "local")
   (zone (zone-file
          (origin "local")
          (entries local.zone)))))



;;;
;;; Support functions
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


;;;
;;; /etc/hosts
;;;

(define (prefix-local-host-aliases prefix host-name domain)
  (string-join (map (lambda (x)
                      (string-append (string-join x " ") "." host-name domain))
                    (cartesian-product '("127.0.0.1" "::1") prefix))
               "\n"))


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
                 (target "/boot/efi")
                 (menu-entries
                  (list (menu-entry
                         (label "NixOS's Grub")
                         (linux "")
                         (initrd "")
                         (additional-options '("search --label --set nixos"
                                               "configfile /boot/grub/grub.cfg")))))))

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
                           (flags '(no-dev))
                           (options "mode=1777,size=16G")
                           (needed-for-boot? #t)
                           (check? #f))
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
                                          "adbusers"))
                  (home-directory "/home/natsu"))
                 %base-user-accounts))

    (hosts-file
     ;; Create a /etc/hosts file with aliases for "localhost"
     ;; and "mymachine", as well as for Facebook servers.
     (plain-file "hosts"
                 (string-append (local-host-aliases host-name)
                                (prefix-local-host-aliases
                                 '("cgit" "guix" "www" "natsu"
                                   "torrent" "print")
                                 host-name ".local")
                                %facebook-host-aliases)))

    ;; Lightweight desktop with custom packages from guix-wigust
    (packages
     (cons*
      desktop-file-utils
      gvfs
      setxkbmap   ; Keyboard layout
      wmctrl      ; `ewmctrl'
      xclip       ; X clipboard CLI
      xdg-utils
      xdotool     ; Mouse and keyboard automation
      xorg-server ; `xephyr'
      xrandr      ; Change screen resolution
      xrdb
      xset
      xsetroot
      xterm       ; $TERM
      xwininfo    ; X Window information
      ;; For helm-stumpwm-commands and stumpish
      rlwrap
      xprop

      (list git "gui")
      (list git "send-email")
      (list git "svn")
      adb       ; For Replicant (Android distribution) control
      cups      ; Printer
      duplicity ; Incremental backup
      ethtool   ; wol (wake on lan)
      file      ; Information about file from magic
      git       ; Version control
      gnupg
      graphviz  ; `dot'
      iptables
      knot
      ncurses
      nss-certs ; for https
      openssh   ; `scp'
      pinentry  ; Password typing for Gnupg
      qemu
      recutils  ; Filter records like in `guix --search'
      rsync
      screen
      strace
      tcpdump
      tree      ; List files as a tree
      wget
      zile
      xkill

      adwaita-icon-theme
      hicolor-icon-theme
      font-awesome
      font-dejavu
      font-liberation
      font-misc-misc  ; for `xterm'
      font-wqy-zenhei ; Chinese, Japanese, Korean
      fontconfig      ; `fc-cache -f'
      ratpoison       ; StumpWM father

      alsa-utils
      pavucontrol ; Pulseaudio control GUI
      pulsemixer  ; Pulseaudio control CLI

      %base-packages))

    (services (cons* firewall-service
                     (static-networking-service "enp6s0"
                                                "192.168.105.120"
                                                #:netmask "255.255.255.0"
                                                #:gateway "192.168.105.1"
                                                #:name-servers '("8.8.8.8"
                                                                 "8.8.4.4"))

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
                               (clone-prefix (list "git://magnolia.local/~natsu"))
                               (nginx (list %cgit-configuration-nginx-custom))))

                     guix-publish-nginx-service
                     cups-nginx-service
                     torrent-nginx-service
                     natsu-nginx-service

                     (simple-service 'adb udev-service-type
                                     (list android-udev-rules %udev-rule-won))

                     (service knot-service-type
                              (knot-configuration
                               (zones (list master-zone))))

                     %custom-desktop-services))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

%system-magnolia

;;; system-magnolia.scm ends here
