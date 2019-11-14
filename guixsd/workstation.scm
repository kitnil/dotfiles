(use-modules (gnu) (majordomo packages majordomo) (guix gexp))
(use-package-modules certs networking)
(use-service-modules desktop networking ssh xorg)

(define %system
  (operating-system
    (host-name "workstation-guixsd")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")))

    (users (cons (user-account (name "oleg")
                               (comment "Oleg Pykhalov")
                               (group "users")
                               (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))

    (file-systems (cons* (file-system
                           (device (uuid "11d541d4-7914-4937-8ce0-7e50687ddbc6"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device "tmpfs")
                           (mount-point "/tmp")
                           (type "tmpfs")
                           (check? #f)
                           (flags '(no-dev))
                           (options "mode=1777,size=50%"))
                         %base-file-systems))

    (packages (cons* nss-certs ;SSL certificates
                     majordomo-ca

                     %base-packages))

    (services (cons (service openssh-service-type) %desktop-services))

    (setuid-programs (cons* (file-append fping "/sbin/fping")
                            (file-append mtr "/sbin/mtr")
                            %setuid-programs))

    (sudoers-file (local-file "sudoers"))))

(list (machine
       (operating-system %system)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "172.16.100.60")
                       (system "x86_64-linux")
                       (user "oleg")
                       (identity "/home/oleg/.ssh/id_rsa")))))
