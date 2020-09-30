;; This is an operating system configuration for a VM image.
;; Modify it as you see fit and instantiate the changes by running:
;;
;;   guix system reconfigure /etc/config.scm
;;

(use-modules (gnu) (guix) (srfi srfi-1))
(use-service-modules desktop networking ssh xorg web)
(use-package-modules bootloaders certs fonts nvi
                     package-management wget xorg)

;; Third-party modules
(use-modules (services webssh))

(define vm-image-motd (plain-file "motd" "
\x1b[1;37mThis is the GNU system.  Welcome!\x1b[0m

This instance of Guix is a template for virtualized environments.
You can reconfigure the whole system by adjusting /etc/config.scm
and running:

  guix system reconfigure /etc/config.scm

Run '\x1b[1;37minfo guix\x1b[0m' to browse documentation.

\x1b[1;33mConsider setting a password for the 'root' and 'guest' \
accounts.\x1b[0m
"))

(operating-system
  (host-name "gnu")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")
  (keyboard-layout (keyboard-layout "us" "altgr-intl"))

  ;; Label for the GRUB boot menu.
  (label (string-append "GNU Guix " (package-version guix)))

  (firmware '())

  ;; Below we assume /dev/vda is the VM's hard disk.
  ;; Adjust as needed.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/vda")
               (terminal-outputs '(console))))
  (file-systems (cons (file-system
                        (mount-point "/")
                        (device "/dev/vda1")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons* (user-account
                (name "oleg")
                (comment "Oleg Pykhalov")
                (password "")                     ;no password
                (uid 1000)
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               (user-account
                (name "guest")
                (comment "GNU Guix Live")
                (password "")                     ;no password
                (group "users")
                (uid 1001)
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Our /etc/sudoers file.  Since 'guest' initially has an empty password,
  ;; allow for password-less sudo.
  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  (packages (append (list font-bitstream-vera nss-certs nvi wget)
                    %base-packages))

  (services
   (append (list (service xfce-desktop-service-type)

                 ;; Choose SLiM, which is lighter than the default GDM.
                 (service slim-service-type
                          (slim-configuration
                           (auto-login? #f)
                           (default-user "oleg")
                           (xorg-configuration
                            (xorg-configuration
                             (keyboard-layout keyboard-layout)))))

                 ;; Uncomment the line below to add an SSH server.
                 (service openssh-service-type)

                 (service nginx-service-type
                          (nginx-configuration
                           (server-blocks (list (nginx-server-configuration
                                                 (inherit %webssh-configuration-nginx)
                                                 (server-name '("webssh.guix.vm.wugi.info"))
                                                 ;; (listen '("443 ssl"))
                                                 ;; (ssl-certificate (letsencrypt-certificate "webssh.wugi.info"))
                                                 ;; (ssl-certificate-key (letsencrypt-key "webssh.wugi.info"))
                                                 (locations
                                                  (cons (nginx-location-configuration
                                                         (uri "/.well-known")
                                                         (body '("root /var/www;")))
                                                        (nginx-server-configuration-locations %webssh-configuration-nginx))))))))

                 (service webssh-service-type
                          (webssh-configuration (address "127.0.0.1")
                                                (port 8888)
                                                (policy 'reject)
                                                (known-hosts '("\
78.108.82.157 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJItpECN9IUeYtH+kaIjrZ//yXmggmebwhg+qBegHwd0kniwYMIrXBGlNKd2uWw6ErhWL/3IMt7FvslBtgwuQ10="
                                                               "\
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJItpECN9IUeYtH+kaIjrZ//yXmggmebwhg+qBegHwd0kniwYMIrXBGlNKd2uWw6ErhWL/3IMt7FvslBtgwuQ10="
                                                               "\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJItpECN9IUeYtH+kaIjrZ//yXmggmebwhg+qBegHwd0kniwYMIrXBGlNKd2uWw6ErhWL/3IMt7FvslBtgwuQ10="))))

                 ;; Use the DHCP client service rather than NetworkManager.
                 (static-networking-service "eth0" "78.108.82.157" #:netmask "255.255.254.0" #:gateway "78.108.83.254" #:name-servers '("8.8.8.8")))

           ;; Remove GDM, ModemManager, NetworkManager, and wpa-supplicant,
           ;; which don't make sense in a VM.
           (remove (lambda (service)
                     (let ((type (service-kind service)))
                       (or (memq type
                                 (list gdm-service-type
                                       wpa-supplicant-service-type
                                       cups-pk-helper-service-type
                                       network-manager-service-type
                                       modem-manager-service-type))
                           (eq? 'network-manager-applet
                                (service-type-name type)))))
                   (modify-services %desktop-services
                     (login-service-type config =>
                                         (login-configuration
                                          (inherit config)
                                          (motd vm-image-motd)))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
