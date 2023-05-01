;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules networking linux nix ssh)
(use-package-modules certs screen ssh)

(use-modules (config)
             (services dns))

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

  ;; Globally-installed packages.
  (packages (cons* nss-certs screen %base-packages))

  ;; Add services to the baseline.
  (services (append (list (static-networking-service "eth0" "78.108.92.69"
                                                     #:netmask "255.255.254.0"
                                                     #:gateway "78.108.93.254"
                                                     #:name-servers '("127.0.0.1"
                                                                      "8.8.8.8"
                                                                      "8.8.4.4"))
                          (service ntp-service-type
                                   (ntp-configuration
                                    (servers
                                     (list
                                      (ntp-server
                                       (type 'pool)
                                       (address "78.108.93.254")
                                       (options '("iburst")))))))
                          (service knot-dns-service-type
                                   (knot-dns-configuration
                                    (config-file
                                     (knot-config "78.108.92.69"))))
                          (service openssh-service-type)
                          (service zram-device-service-type
                                   (zram-device-configuration
                                    (size "8G")))
                          (service nix-service-type
                                   (nix-configuration
                                    (extra-config '("trusted-users = oleg root")))))
                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config-with-substitute-urls))))

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n")))
