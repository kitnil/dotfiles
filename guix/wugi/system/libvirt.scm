;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

(define-module (wugi system libvirt)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu services base)
  #:use-module (gnu services virtualization)
  #:export (%libvirt))

(define (%libvirt)
  (define syslogd
    (program-file
     "syslogd"
     #~(begin
         (use-modules (srfi srfi-1))
         (let ((args (cdr (command-line)))
               (extra-options '("--rcfile=/etc/syslog.conf"
                                "--no-forward"
                                "--no-unixaf"
                                "--no-klog")))
           (apply execl
                  (append (list #$(file-append inetutils "/libexec/syslogd")
                                "syslogd")
                          args
                          extra-options))))))

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

    (packages (append (list lvm2)
                      %base-packages))

    (services
     (list
      ;; The LVM2 rules are needed as soon as LVM2 or the device-mapper is
      ;; used, so enable them by default.  The FUSE and ALSA rules are
      ;; less critical, but handy.
      (service udev-service-type
               (udev-configuration
                (rules (list lvm2))))
      (service openssh-service-type
               (openssh-configuration
                (authorized-keys
                 `(("root" ,(local-file
                             (string-append
                              %distro-directory
                              "/dotfiles/guixsd/ssh/id_rsa_pc0.pub")))))
                (permit-root-login 'prohibit-password)
                (password-authentication? #f)
                (use-pam? #f)))
      (service special-files-service-type
               `(("/bin/sh" ,(file-append bash "/bin/sh"))
                 ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))
      (service syslog-service-type
               (syslog-configuration
                (syslogd syslogd)))
      (service libvirt-service-type
               (libvirt-configuration
                (listen-tcp? #t)
                (auth-tcp "none")))
      (service virtlog-service-type
               (virtlog-configuration
                (max-clients 1000)))))))
