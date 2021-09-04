;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules certbot databases dbus desktop docker monitoring networking ssh web)
(use-package-modules curl certs screen ssh)

(use-modules (config))

(operating-system
  (host-name "vm4.wugi.info")
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

  (groups (append (list (user-group
                         (name "docker")
                         (system? #t)))
                  %base-groups))

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
                (supplementary-groups '("wheel" "audio" "video" "docker")))
               %base-user-accounts))

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  ;; Globally-installed packages.
  (packages (cons* curl nss-certs screen %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append (list (static-networking-service "eth0" "78.108.82.44"
                                                     #:netmask "255.255.254.0"
                                                     #:gateway "78.108.83.254"
                                                     #:name-servers '("8.8.8.8" "8.8.4.4"))
                          (service openssh-service-type
                                   (openssh-configuration
                                    (password-authentication? #t)
                                    (use-pam? #f)))
                          (dbus-service)
                          (elogind-service)
                          (service docker-service-type)
                          (service nginx-service-type
                                   (nginx-configuration
                                    (server-blocks (list (proxy "file.wugi.info" 5091 #:ssl? #t #:ssl-key? #t)))))
                          (service certbot-service-type
                                   (certbot-configuration
                                    (email "go.wigust@gmail.com")
                                    (certificates
                                     `(,@(map (lambda (host)
                                                (certificate-configuration
                                                 (domains (list host))
                                                 (deploy-hook %nginx-deploy-hook)))
                                              (list "file.wugi.info"))))))
                          (service zabbix-agent-service-type %vm-zabbix-agent-configuration)
                          (service prometheus-node-exporter-service-type))
                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config-with-substitute-urls)))))
