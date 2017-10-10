;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu) (gnu system nss))

(use-service-modules ssh desktop xorg cups version-control mail
                     networking shepherd rsync web)

(use-package-modules bootloaders emacs cups certs cryptsetup ssh guile
                     package-management bash linux android version-control)


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

       ;; Rules to accept web traffic only on private network.
       (iptables "-A INPUT -p tcp --dport 80 -s 192.168.0.0/16 -j ACCEPT")
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
  (modify-services %desktop-services
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
    (list
     (nginx-server-configuration
      (server-name '("www.magnolia.local"))
      (root "/srv/share")
      (https-port #f)
      (ssl-certificate #f)
      (ssl-certificate-key #f))))))

(define %cgit-configuration-nginx
  (list
   (nginx-server-configuration
    (root cgit)
    (server-name `("cgit.magnolia.local"))
    (locations
     (list
      (nginx-location-configuration
       (uri "@cgit")
       (body '("fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
               "fastcgi_param PATH_INFO $uri;"
               "fastcgi_param QUERY_STRING $args;"
               "fastcgi_param HTTP_HOST $server_name;"
               "fastcgi_pass 127.0.0.1:9000;")))))
    (try-files (list "$uri" "@cgit"))
    (https-port #f)
    (ssl-certificate #f)
    (ssl-certificate-key #f))))


;;;
;;; Operating system.
;;;

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
                (supplementary-groups '("wheel" "audio" "video" "lpadmin" "lp"
                                        "adbusers"))
                (home-directory "/home/natsu"))
               %base-user-accounts))

  (hosts-file
   ;; Create a /etc/hosts file with aliases for "localhost"
   ;; and "mymachine", as well as for Facebook servers.
   (plain-file "hosts"
               (string-append (local-host-aliases host-name)
                              "127.0.0.1 www." host-name ".local" "\n"
                              "127.0.0.1 cgit." host-name ".local" "\n"
                              %facebook-host-aliases)))

  (packages (cons* cups cryptsetup emacs emacs-guix guile-ssh guix
                   nss-certs iptables openssh
                   %base-packages))

  (services (cons* (service openssh-service-type
                            (openssh-configuration
                             (port-number 22)))
                   ;; Configure CUPS on https://localhost:631
                   ;; and be sure librejs is disabled in browser
                   (service cups-service-type
                            (cups-configuration
                             (web-interface? #t)
                             (extensions
                              (list cups-filters hplip))))
                   (dovecot-service
                    #:config (dovecot-configuration
                              (mail-location
                               (string-append
                                "maildir:~/Maildir:INBOX=~/Maildir/INBOX:"
                                "LAYOUT=fs"))
                              (disable-plaintext-auth? #f)
                              (listen '("127.0.0.1"))))
                   (service guix-publish-service-type
                            (guix-publish-configuration
                             (host "0.0.0.0")
                             (port 3000)))
                   (service git-daemon-service-type
                            (git-daemon-configuration
                             (user-path "")))
                   (service rsync-service-type)
                   (service nginx-service-type %file-share-configuration-nginx)
                   (service fcgiwrap-service-type)
                   (tor-service)
                   (service cgit-service-type
                            (cgit-configuration
                             (nginx %cgit-configuration-nginx)))
                   (simple-service 'adb udev-service-type (list android-udev-rules))
                   firewall-service
                   %custom-desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
