(use-modules (gnu)
             (guix)
             (srfi srfi-1)
             (srfi srfi-26))

(use-service-modules certbot desktop networking monitoring ssh vpn xorg web)

(use-package-modules admin base bootloaders certs package-management wget xorg zile)

;; Third-party modules
(use-modules (config)
             (services kresd))

(operating-system
  (host-name "guix.vm.wugi.info")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

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

  (groups (cons* (user-group (name "nixbld")
                             (id 30100))
                 %base-groups))

  (users (cons* (user-account
                 (name "oleg")
                 (comment "Oleg Pykhalov")
                 (uid 1000)
                 (group "users")
                 (supplementary-groups '("wheel" "netdev"
                                         "audio" "video")))
                (append ((lambda* (count #:key
                                    (group "nixbld")
                                    (first-uid 30101)
                                    (shadow shadow))
                           (unfold (cut > <> count)
                                   (lambda (n)
                                     (user-account
                                      (name (format #f "nixbld~a" n))
                                      (system? #t)
                                      (uid (+ first-uid n -1))
                                      (group group)

                                      ;; guix-daemon expects GROUP to be listed as a
                                      ;; supplementary group too:
                                      ;; <http://lists.gnu.org/archive/html/bug-guix/2013-01/msg00239.html>.
                                      (supplementary-groups (list group "kvm"))

                                      (comment (format #f "Nix Build User ~a" n))
                                      (home-directory "/var/empty")
                                      (shell (file-append shadow "/sbin/nologin"))))
                                   1+
                                   1))
                         9)
                        %base-user-accounts)))

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  (packages %my-system-packages)

  (services
   (append (list (service openssh-service-type
                          (openssh-configuration
                           (x11-forwarding? #t)
                           (gateway-ports? 'client)
                           (password-authentication? #f)
                           (extra-content "\
Match Address 127.0.0.1
PasswordAuthentication yes")))
                 (static-networking-service "eth0" "78.108.82.157"
                                            #:netmask "255.255.254.0"
                                            #:gateway "78.108.83.254"
                                            #:name-servers '("78.108.82.157\nsearch intr majordomo.ru"
                                                             "172.17.0.1"
                                                             "8.8.8.8"
                                                             "8.8.4.4"))
                 (extra-special-file "/usr/bin/env"
                                     (file-append coreutils "/bin/env"))
                 (kresd-service (local-file "kresd.conf"))
                 (service zabbix-agent-service-type
                          (zabbix-agent-configuration
                           (server '("back.wugi.info"))
                           (server-active '("back.wugi.info"))))
                 (openvpn-client-service
                  #:config (openvpn-client-configuration
                            (dev 'tap)
                            (auth-user-pass "/etc/openvpn/login.conf")
                            (remote (list
                                     ;; vpn-miran.majordomo.ru
                                     (openvpn-remote-configuration
                                      (name "78.108.80.230"))
                                     ;; vpn-dh.majordomo.ru
                                     (openvpn-remote-configuration
                                      (name "78.108.91.250"))
                                     ;; vpn-office.majordomo.ru
                                     (openvpn-remote-configuration
                                      (name "81.95.28.29"))))))

                 (service certbot-service-type
                          (certbot-configuration
                           (email "go.wigust@gmail.com")
                           (certificates
                            `(,@(map (lambda (host)
                                       (certificate-configuration
                                        (domains (list host))
                                        (deploy-hook %nginx-deploy-hook)))
                                     (list "vm1.wugi.info"))))))

                 (service nginx-service-type
                          (nginx-configuration
                           (server-blocks (list (nginx-server-configuration
                                                 (inherit %webssh-configuration-nginx)
                                                 (server-name '("vm1.wugi.info"))
                                                 (listen '("443 ssl"))
                                                 (ssl-certificate (letsencrypt-certificate "vm1.wugi.info"))
                                                 (ssl-certificate-key (letsencrypt-key "vm1.wugi.info"))
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
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJItpECN9IUeYtH+kaIjrZ//yXmggmebwhg+qBegHwd0kniwYMIrXBGlNKd2uWw6ErhWL/3IMt7FvslBtgwuQ10="
                                                               "\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJItpECN9IUeYtH+kaIjrZ//yXmggmebwhg+qBegHwd0kniwYMIrXBGlNKd2uWw6ErhWL/3IMt7FvslBtgwuQ10=")))))
           (load "desktop.scm")
           %base-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
