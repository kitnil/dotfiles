;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system image -t iso9660 dotfiles/guixsd/modules/system/install.scm

(define-module (system install)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (nongnu packages linux)
  #:use-module (gnu)
  #:export (installation-os-nonfree))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))
    (packages
     (append (list curl git netcat-openbsd rsync tmux)
             (operating-system-packages installation-os)))
    (services
     (modify-services (operating-system-user-services installation-os)
       (guix-service-type
        config =>
        (guix-configuration
         (inherit config)
         (substitute-urls '("http://ci.guix.trop.in"
                            "https://bordeaux.guix.gnu.org"
                            "https://substitutes.nonguix.org"))))))))

installation-os-nonfree
