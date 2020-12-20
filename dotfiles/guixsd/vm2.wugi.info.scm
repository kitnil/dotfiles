;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules certbot networking monitoring ssh web)
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
                          (service openssh-service-type
                                   (openssh-configuration
                                    (x11-forwarding? #t)
                                    (gateway-ports? 'client)
                                    (password-authentication? #f)
                                    (extra-content "\
Match Address 127.0.0.1
PasswordAuthentication yes")))
                          (service keepalived-service-type
                                   (keepalived-configuration
                                    (config-file (local-file "etc/keepalived/vm2.wugi.info.conf"))))
                          (service gre-service-type
                                   (gre-configuration
                                    (ip-address-local "78.108.87.161")
                                    (ip-address-remote "78.108.82.157")
                                    (ip-address "10.0.0.2/24")
                                    (interface-name "gre1")))
                          (service certbot-service-type
                          (certbot-configuration
                           (email "go.wigust@gmail.com")
                           (certificates
                            `(,@(map (lambda (host)
                                       (certificate-configuration
                                        (domains (list host))
                                        (deploy-hook %nginx-deploy-hook)))
                                     (list "vm2.wugi.info"))))))

                          (service nginx-service-type
                                   (nginx-configuration
                                    (modules %nginx-modules)
                                    (lua-package-path %nginx-lua-package-path)
                                    (lua-package-cpath %nginx-lua-package-cpath)
                                    (server-blocks (list (nginx-server-configuration
                                                          (inherit %webssh-configuration-nginx)
                                                          (server-name '("vm2.wugi.info"))
                                                          (listen '("443 ssl"))
                                                          (ssl-certificate (letsencrypt-certificate "vm2.wugi.info"))
                                                          (ssl-certificate-key (letsencrypt-key "vm2.wugi.info"))
                                                          (locations
                                                           (append %nginx-lua-guix
                                                                   (cons (nginx-location-configuration
                                                                          (uri "/.well-known")
                                                                          (body '("root /var/www;")))
                                                                         (nginx-server-configuration-locations %webssh-configuration-nginx)))))))))

                          (service webssh-service-type
                                   (webssh-configuration (address "127.0.0.1")
                                                         (port 8888)
                                                         (policy 'reject)
                                                         (known-hosts '("\
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHVSCVdQEHUaTnBqA2nKQXRmo/74DgnyCyWiOI/f5G7qYUMfDiJqYHqh7YngyxIG9iakEUOaNtr6ljHyBXhlaPQ="
                                                                        "\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHVSCVdQEHUaTnBqA2nKQXRmo/74DgnyCyWiOI/f5G7qYUMfDiJqYHqh7YngyxIG9iakEUOaNtr6ljHyBXhlaPQ=")))))
                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config)))))
