;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules certbot databases monitoring networking ssh web)
(use-package-modules certs screen ssh)

(use-modules (config))

(operating-system
  (host-name "vm3.wugi.info")
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
  (services (append (list (static-networking-service "eth0" "78.108.87.14"
                                                     #:netmask "255.255.254.0"
                                                     #:gateway "78.108.87.254"
                                                     #:name-servers '("8.8.8.8" "8.8.4.4"))
                          (service zabbix-agent-service-type %vm-zabbix-agent-configuration)
                          (service openssh-service-type)
                          (service certbot-service-type
                                   (certbot-configuration
                                    (email "go.wigust@gmail.com")
                                    (certificates
                                     `(,@(map (lambda (host)
                                                (certificate-configuration
                                                 (domains (list host))
                                                 (deploy-hook %nginx-deploy-hook)))
                                              (list "zabbix.wugi.info"))))))
                          (postgresql-service #:config-file (postgresql-config-file
                                                             (hba-file
                                                              (plain-file "pg_hba.conf"
                                                                          "
local	all	all			trust
host	all	all	127.0.0.1/32    trust
host	all	all	::1/128         trust
host	all	all	172.16.0.0/12   trust"))
                                                             (extra-config '(("listen_addresses" "'0.0.0.0'")))))
                          (service zabbix-server-service-type
                                   (zabbix-server-configuration
                                    (include-files '("/etc/zabbix/zabbix-server.secret"))
                                    (extra-options "
AlertScriptsPath=/etc/zabbix/alertscripts
ExternalScripts=/etc/zabbix/externalscripts
FpingLocation=/run/setuid-programs/fping
")))
                          (service zabbix-front-end-service-type
                                (zabbix-front-end-configuration
                                 (db-secret-file "/etc/zabbix/zabbix.secret")
                                 (nginx %zabbix-nginx-configuration))))
                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config)))))
