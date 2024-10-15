;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

(use-modules (gnu)
             (gnu packages base)
             (gnu packages bash)
             (gnu services networking))

(operating-system
  (host-name "isc-dhcp")
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

  (services (list (syslog-service)
                  (service special-files-service-type
                           `(("/bin/sh" ,(file-append bash "/bin/sh"))
                             ("/usr/bin/env" ,(file-append coreutils "/bin/env")))))))
