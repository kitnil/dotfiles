;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

(define-module (wugi system libvirt)
  #:use-module (gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu services virtualization)
  #:export (%libvirt))

(define (%libvirt)
  (operating-system
    (host-name "libvirt")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

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

    (services
     (list
      (service special-files-service-type
               `(("/bin/sh" ,(file-append bash "/bin/sh"))
                 ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))
      (service libvirt-service-type
               (libvirt-configuration
                (listen-tcp? #t)
                (auth-tcp "none")))
      (service virtlog-service-type
               (virtlog-configuration
                (max-clients 1000)))))))
