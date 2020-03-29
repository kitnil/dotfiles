(define-module (fiore install)
  #:use-module (gnu system install)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xfce)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu packages cryptsetup)
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
                     gnupg openssh parted pinentry-tty
                     password-store cryptsetup
                     (operating-system-packages installation-os)))
    (setuid-programs %setuid-programs)))

%fiore-installation-os
