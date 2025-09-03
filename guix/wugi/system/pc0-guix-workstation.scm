;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system image -t docker --network ./docker-image-workstation.scm
;; skopeo copy docker-archive:/gnu/store/…-tor-docker-pack.tar.gz docker-daemon:example.org:5000/tor:latest --insecure-policy
;; docker run --network=host --security-opt seccomp=unconfined --detach --name tor --network=host example.org:5000/tor
;; docker exec --detach tor /gnu/store/…-tor-0.4.6.10/bin/tor -f /gnu/store/…-torrc

(define-module (wugi system pc0-guix-workstation)
  #:use-module (wugi services desktop)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sound)
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
  #:use-module (wugi config)
  #:use-module (wugi utils)
  #:use-module (wugi utils package)
  #:export (%pc0-guix-workstation))

(define (%pc0-guix-workstation)
  (define container-mingetty-service-type
    (service-type (name 'mingetty)
                  (extensions (list (service-extension shepherd-root-service-type
                                                       (@@ (gnu services base) mingetty-shepherd-service))))
                  (description
                   "Provide console login using the @command{mingetty}
program.")))

  (define %my-operating-system
    (operating-system
      (host-name "pc0-guix-workstation")
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
         (service syslog-service-type
                  (syslog-configuration
                   (extra-options '("--rcfile=/etc/syslog.conf"
                                    "--no-forward"
                                    "--no-unixaf"
                                    "--no-klog"))))
         (service elogind-service-type)
         seatd-service
         (service dbus-root-service-type)
         (service container-mingetty-service-type
                  (mingetty-configuration (tty "tty8")))
         (service bluetooth-service-type
                  (bluetooth-configuration
                   ;; (bluez
                   ;;  (package-from-program-file
                   ;;   (program-file "bluetoothd"
                   ;;                 #~(begin
                   ;;                     (execl #$(file-append util-linux+udev "/bin/nsenter")
                   ;;                            "--net=/rootns/net"
                   ;;                            "--"
                   ;;                            #$(file-append bluez "/libexec/bluetooth/bluetoothd") "--debug" "--nodetach")))
                   ;;   "/libexec/bluetooth"))
                   (auto-enable? #t)
                   (just-works-repairing 'confirm)
                   (controller-mode 'dual)
                   (min-connection-interval 7)
                   (max-connection-interval 9)
                   (connection-latency 0)
                   (privacy 'device)))
         udev-rules-service-xbox
         (service ladspa-service-type
                  (ladspa-configuration (plugins (list swh-plugins))))
         (service avahi-service-type))
        (modify-services %base-services
          (delete shepherd-system-log-service-type)
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
            (substitute-urls '("https://mirrors.sjtug.sjtu.edu.cn/guix"
                               "https://substitutes.nonguix.org")))))))

      (sudoers-file (plain-file "sudoers"
                                (string-join `("Defaults:root runcwd=*"
                                               "root ALL=(ALL) ALL"
                                               "%wheel ALL=(ALL) ALL"
                                               "oleg ALL=(ALL) NOPASSWD:ALL")
                                             "\n")))))

  %my-operating-system)
