(use-modules (gnu) (guix) (srfi srfi-1))
(use-package-modules bash ssh)
(use-service-modules ssh)

(operating-system
  (host-name "gnu")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/vda")
               (terminal-outputs '(console))))

  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda1")
                        (type "ext4"))
                      %base-file-systems))

  (packages (list bash openssh))

  (services (append (list (service openssh-service-type
                                   (openssh-configuration
                                    (authorized-keys
                                     `(("root" ,(local-file "/home/oleg/.ssh/id_rsa_pool.pub"))))
                                    (permit-root-login #t)
                                    (gateway-ports? 'client)
                                    (password-authentication? #f))))
                    %base-services)))
