;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system image -t docker --network ./docker-image-workstation.scm
;; skopeo copy docker-archive:/gnu/store/…-tor-docker-pack.tar.gz docker-daemon:example.org:5000/tor:latest --insecure-policy
;; docker run --network=host --security-opt seccomp=unconfined --detach --name tor --network=host example.org:5000/tor
;; docker exec --detach tor /gnu/store/…-tor-0.4.6.10/bin/tor -f /gnu/store/…-torrc

(define-module (wugi system docker-image)
  #:use-module (wugi services desktop)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system linux-container)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels docker-image)
  #:use-module (wugi utils)
  #:export (%docker-image))

(define (%docker-image)
  (define container-mingetty-service-type
    (service-type (name 'mingetty)
                  (extensions (list (service-extension shepherd-root-service-type
                                                       (@@ (gnu services base) mingetty-shepherd-service))))
                  (description
                   "Provide console login using the @command{mingetty}
program.")))

  (define %my-operating-system
    (operating-system
      (host-name "workstation")
      (timezone "Europe/Moscow")
      (locale "en_US.utf8")

      ;; This is where user accounts are specified.  The "root" account is
      ;; implicit, and is initially created with the empty password.
      (users (append (list (user-account
                            (name "oleg")
                            (comment "Oleg Pykhalov")
                            (group "users")
                            (supplementary-groups '("wheel"
                                                    "audio"
                                                    "video"
                                                    "kvm"
                                                    "input"))
                            (password (crypt "oleg" "NmhJoj")))
                           (user-account (inherit %root-account)
                                         (password (crypt "root" "uUxBgD"))))
                     %base-user-accounts))

      ;; Globally-installed packages.
      (packages (append (list bash-completion openssh)
                        %base-packages))

      ;; Because the system will run in a Docker container, we may omit many
      ;; things that would normally be required in an operating system
      ;; configuration file.  These things include:
      ;;
      ;;   * bootloader
      ;;   * file-systems
      ;;   * services such as mingetty, udevd, slim, networking, dhcp
      ;;
      ;; Either these things are simply not required, or Docker provides
      ;; similar services for us.

      ;; This will be ignored.
      (bootloader (bootloader-configuration
                   (bootloader grub-bootloader)
                   (targets '("does-not-matter"))))

      ;; This will be ignored, too.
      (file-systems (list (file-system
                            (device "does-not-matter")
                            (mount-point "/")
                            (type "does-not-matter"))))

      ;; Guix is all you need!
      (services
       (append
        (list
         (service elogind-service-type)
         seatd-service
         (service dbus-root-service-type))
        (modify-services %base-services
          (guix-service-type
           config =>
           (guix-configuration
            (authorized-keys
             (append
              (map (lambda (file-name)
                     (local-file
                      (string-append %distro-directory
                                     "/wugi/etc/substitutes/" file-name)))
                   '("bordeaux.guix.gnu.org.pub"
                     "guix-builder.pub"
                     "guix.wugi.info.pub"
                     "mirror.brielmaier.net.pub"
                     "substitutes.nonguix.org.pub"
                     "vm1.wugi.info.pub"
                     "vm2.wugi.info.pub"))
              %default-authorized-guix-keys))
            (substitute-urls '("http://runc-kube1-guix-builder.guix:5556"
                               "http://guix.localhost"
                               "http://nonguix.localhost")))))))

      (sudoers-file (plain-file "sudoers"
                                (string-join `("Defaults:root runcwd=*"
                                               "root ALL=(ALL) ALL"
                                               "%wheel ALL=(ALL) ALL"
                                               "oleg ALL=(ALL) NOPASSWD:ALL")
                                             "\n")))))

  %my-operating-system)
