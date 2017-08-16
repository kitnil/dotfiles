;; GuixSD configuration file for the desktop machine.
;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(use-modules (gnu) (gnu system nss))

(use-service-modules ssh desktop xorg cups pm version-control admin mcron mail
                     networking shepherd rsync cuirass)

(use-package-modules bootloaders emacs cups wm certs fonts xdisorg cryptsetup
                     ssh guile package-management bash linux)


;;;
;;; Firewall service
;;;

(define start-firewall
  ;; Rules to throttle malicious SSH connection attempts.  This will allow at
  ;; most 3 connections per minute from any host, and will block the host for
  ;; another minute if this rate is exceeded.  Taken from
  ;; <http://www.la-samhna.de/library/brutessh.html#3>.
  #~(let ((iptables
           (lambda (str)
             (zero? (apply system*
                           #$(file-append iptables
                                          "/sbin/iptables")
                           (string-tokenize str))))))
      (format #t "Installing iptables SSH rules...~%")
      (and (iptables "-A INPUT -p tcp --dport 22 -m state \
  --state NEW -m recent --set --name SSH -j ACCEPT")
           (iptables "-A INPUT -p tcp --dport 22 -m recent \
  --update --seconds 60 --hitcount 4 --rttl \
  --name SSH -j LOG --log-prefix SSH_brute_force")
           (iptables "-A INPUT -p tcp --dport 22 -m recent \
  --update --seconds 60 --hitcount 4 --rttl --name SSH -j DROP"))))

(define firewall-service
  ;; The "firewall".  Make it a Shepherd service because as an activation
  ;; script it might run too early, before the Netfilter modules can be
  ;; loaded for some reason.
  (simple-service 'firewall shepherd-root-service-type
                  (list (shepherd-service
                         (provision '(firewall))
                         (requirement '())
                         (start #~(lambda ()
                                    #$start-firewall))
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
                                  (auto-login? #t)
                                  (default-user "natsu")))))


;;;
;;; Cuirass.
;;;

(define %cuirass-specs
  ;; Cuirass specifications to build Guix.
  #~(list `((#:name . "guix")
            (#:url . "git://magnolia.local/~natsu/src/guix")
            (#:load-path . ".")

            ;; FIXME: Currently this must be an absolute file name because
            ;; the 'evaluate' command of Cuirass loads it with
            ;; 'primitive-load'.
            ;; Use our own variant of Cuirass' 'examples/gnu-system.scm'.
            (#:file . #$(local-file "cuirass-jobs.scm"))
            (#:no-compile? #t)      ;don't try to run ./bootstrap etc.

            (#:proc . hydra-jobs)
            (#:arguments (subset . "core"))
            (#:branch . "master"))))


;;;
;;; Operating system.
;;;

(operating-system
  (host-name "magnolia")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (grub-configuration (grub grub-efi)
                                  (device "/dev/sda")))

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
                         (flags '(no-suid no-dev))
                         (options "mode=1777,size=16G")
                         (needed-for-boot? #t)
                         (check? #f))
                       %base-file-systems))

  (users (cons (user-account
                (name "natsu")
                (uid 1000)
                (comment "Oleg Pykhalov")
                (group "users")
                (supplementary-groups '("wheel" "audio" "video" "lpadmin" "lp"))
                (home-directory "/home/natsu"))
               %base-user-accounts))

  (packages (cons* i3-wm i3status rofi
                   cups
                   cryptsetup
                   emacs emacs-guix
                   guile-2.2 guile-ssh guix
                   nss-certs
                   font-dejavu font-liberation
                   iptables openssh
                   %base-packages))

  (services (cons* (service openssh-service-type
                            (openssh-configuration
                             (port-number 22)))
                   ;; https://localhost:631
                   ;; be sure librejs is disabled in browser
                   (service cups-service-type
                            (cups-configuration
                             (web-interface? #t)
                             (extensions
                              (list cups-filters hplip))))
                   (service guix-publish-service-type
                            (guix-publish-configuration
                             (host "0.0.0.0")))
                   (service git-daemon-service-type
                            (git-daemon-configuration
                             (user-path "")))
                   (dovecot-service
                    #:config (dovecot-configuration
                              (mail-location
                               (string-append
                                "maildir:~/Maildir:INBOX=~/Maildir/INBOX:"
                                "LAYOUT=fs"))
                              (disable-plaintext-auth? #f)
                              (listen '("127.0.0.1"))))
                   (service mcron-service-type)
                   (service rottlog-service-type)
                   (service bitlbee-service-type
                            (bitlbee-configuration))
                   (service rsync-service-type
                            (rsync-configuration))
                   firewall-service
                   (service cuirass-service-type
                            (cuirass-configuration
                             (use-substitutes? #t)
                             ;; (load-path '("/home/natsu/src/guix/packages"))
                             (specifications %cuirass-specs)))
                   %custom-desktop-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
