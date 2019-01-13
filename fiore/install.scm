(define-module (fiore install)
  #:use-module (gnu system install)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xfce)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (srfi srfi-1)
  #:export (%fiore-installation-os))

(define %fiore-installation-os
  (operating-system
    (inherit installation-os)
    (timezone "Europe/Moscow")
    (packages (cons* curl openssh
                     ncurses ;reset
                     tcpdump strace tmux
                     e2fsprogs
                     emacs emacs-guix geiser
                     git gnupg openssh parted password-store pinentry-tty
                     restic
                     (operating-system-packages installation-os)))
    (users (cons (user-account
                  (name "natsu")
                  (comment "Oleg Pykhalov")
                  (group "users")
                  (supplementary-groups '("wheel" "netdev"
                                          "audio" "video"))
                  (home-directory "/home/natsu")
                  (password (crypt "***REMOVED***" "bar")))
                 (operating-system-users installation-os)))
    (services (append (list (xfce-desktop-service))
                      (modify-services (remove (lambda (service)
                                                 (or (eq? (service-kind service)
                                                          udev-service-type)
                                                     (eq? (service-kind service)
                                                          nscd-service-type)
                                                     (eq? (service-kind service)
                                                          static-networking-service-type)
                                                     (eq? (service-kind service)
                                                          console-font-service-type)
                                                     (eq? (service-kind service)
                                                          guix-service-type)
                                                     (eq? (service-kind service)
                                                          mingetty-service-type)
                                                     (eq? (service-kind service)
                                                          virtual-terminal-service-type)
                                                     (eq? (service-kind service)
                                                          pam-root-service-type)
                                                     (eq? (service-kind service)
                                                          login-service-type)))
                                               (operating-system-user-services installation-os)))
                      (modify-services (modify-services (remove (lambda (service)
                                                                  (eq? (service-kind service)
                                                                       syslog-service-type))
                                                                %desktop-services))
                        (special-files-service-type config => `(("/bin/sh"
                                                                 ,(file-append
                                                                   bash "/bin/sh"))
                                                                ("/usr/bin/env"
                                                                 ,(file-append
                                                                   coreutils "/bin/env"))))
                        (slim-service-type config => (slim-configuration
                                                      (inherit config)
                                                      (auto-login? #t)
                                                      (auto-login-session (file-append xfce
                                                                                       "/bin/startxfce4"))
                                                      (default-user "natsu"))))))
    (setuid-programs %setuid-programs)))

%fiore-installation-os
