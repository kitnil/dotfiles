;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules networking monitoring ssh)
(use-package-modules certs screen ssh)

(use-modules (config)
             (services keepalived)
             (services networking))

(operating-system
  (host-name "vm2.wugi.info")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

  ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
  ;; target hard disk, and "my-root" is the label of the target
  ;; root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/vda")))
  (file-systems (cons (file-system
                        (device (file-system-label "guix-root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
                (name "oleg")
                (comment "Oleg Pykhalov")
                (group "users")

                ;; Adding the account to the "wheel" group
                ;; makes it a sudoer.  Adding it to "audio"
                ;; and "video" allows the user to play sound
                ;; and access the webcam.
                (supplementary-groups '("wheel"
                                        "audio" "video")))
               %base-user-accounts))

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  ;; Globally-installed packages.
  (packages (cons* screen nss-certs %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append (list (static-networking-service "eth0" "78.108.87.161"
                                                     #:netmask "255.255.254.0"
                                                     #:gateway "78.108.87.254"
                                                     #:name-servers '("8.8.8.8" "8.8.4.4"))
                          (service zabbix-agent-service-type %vm-zabbix-agent-configuration)
                          (service openssh-service-type)
                          (service keepalived-service-type
                                   (keepalived-configuration
                                    (config-file (local-file "etc/keepalived/vm2.wugi.info.conf"))))
                          (service gre-service-type
                                   (gre-configuration
                                    (ip-address-local "78.108.87.161")
                                    (ip-address-remote "78.108.82.157")
                                    (ip-address "10.0.0.2/24")
                                    (interface-name "gre1"))))
                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config)))))
