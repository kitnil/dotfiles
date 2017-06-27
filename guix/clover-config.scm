;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules admin)

(operating-system
  (host-name "clover")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  (bootloader (grub-configuration (device "/dev/sda")))
  (file-systems (cons (file-system
                        (device "clover-root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "natsu")
                (uid 1000)
                (comment "Oleg Pykhalov")
                (group "users")
                (supplementary-groups '("wheel"
                                        "audio" "video"))
                (home-directory "/home/natsu"))
               %base-user-accounts))

  ;; Globally-installed packages.
  (packages %base-packages)

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (cons* (dhcp-client-service)
                   (service openssh-service-type
                            (openssh-configuration
                              (port-number 22)))
                   %base-services)))
