(use-modules (gnu)
             (guix)
             (srfi srfi-1)
             (srfi srfi-26))

(use-service-modules certbot desktop networking monitoring ssh sysctl vpn xorg web)

(use-package-modules admin base bootloaders certs package-management wget xorg zile)

;; Third-party modules
(use-modules (config)
             (services autossh)
             (services homer)
             (services kresd)
             (services keepalived)
             (services networking)
             (services openvpn))

(operating-system
  (host-name "vm1.wugi.info")
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

                 (service (@ (services autossh) autossh-service-type)
                          ((@ (services autossh) autossh-configuration)
                             (autossh-client-config
                              (autossh-client-configuration
                               (hosts (list (autossh-client-host-configuration
                                             (host "back.wugi.info")
                                             (identity-file "/etc/autossh/id_rsa")
                                             (strict-host-key-checking? #f)
                                             (user "vm1-ssh-tunnel")
                                             (user-known-hosts-file "/dev/null")
                                             (extra-options
                                              "
RemoteForward 0.0.0.0:17022 127.0.0.1:22
RemoteForward 0.0.0.0:17050 127.0.0.1:10050
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                             (host "back.wugi.info")))

                 (extra-special-file "/usr/bin/env"
                                     (file-append coreutils "/bin/env"))
                 (kresd-service (local-file "kresd.conf"))
                 (service zabbix-agent-service-type
                          (zabbix-agent-configuration
                           (server '("zabbix.wugi.info"))
                           (server-active '("zabbix.wugi.info"))))
                 (service prometheus-node-exporter-service-type)

                 (service openvpn-service-type
                          (openvpn-configuration
                           (name "majordomo.ru")
                           (config "/etc/openvpn/openvpn.conf")))

                 (service openvpn-service-type
                          (openvpn-configuration
                           (name "wugi.info")
                           (config (plain-file "openvpn.conf"
                                               "\
proto udp
dev tun
ca /etc/openvpn/ca.crt
cert /etc/openvpn/client.crt
key /etc/openvpn/client.key
duplicate-cn
comp-lzo
persist-key
persist-tun
verb 3
port 1195
server 10.9.0.0 255.255.255.0
dh /etc/openvpn/dh2048.pem
ifconfig-pool-persist /etc/openvpn/ipp.txt
client-to-client
keepalive 5 10
max-clients 100
status /var/run/openvpn/status
push \"route 10.0.0.0 255.255.255.0\"
"))))

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
                           (modules %nginx-modules)
                           (lua-package-path %nginx-lua-package-path)
                           (lua-package-cpath %nginx-lua-package-cpath)
                           (server-blocks (list (nginx-server-configuration
                                                 (inherit %webssh-configuration-nginx)
                                                 (server-name '("vm1.wugi.info"))
                                                 (listen '("443 ssl"))
                                                 (ssl-certificate (letsencrypt-certificate "vm1.wugi.info"))
                                                 (ssl-certificate-key (letsencrypt-key "vm1.wugi.info"))
                                                 (locations
                                                  (append %nginx-lua-guix
                                                          (cons (nginx-location-configuration
                                                                 (uri "/.well-known")
                                                                 (body '("root /var/www;")))
                                                                (nginx-server-configuration-locations %webssh-configuration-nginx)))))
                                                %githunt-nginx-configuration
                                                %homer-nginx-configuration))))

                 (service homer-service-type %homer-config)
                 (service webssh-service-type
                          (webssh-configuration (address "127.0.0.1")
                                                (port 8888)
                                                (policy 'reject)
                                                (known-hosts '("\
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJItpECN9IUeYtH+kaIjrZ//yXmggmebwhg+qBegHwd0kniwYMIrXBGlNKd2uWw6ErhWL/3IMt7FvslBtgwuQ10="
                                                               "\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBJItpECN9IUeYtH+kaIjrZ//yXmggmebwhg+qBegHwd0kniwYMIrXBGlNKd2uWw6ErhWL/3IMt7FvslBtgwuQ10="))))
                 (service keepalived-service-type
                          (keepalived-configuration
                           (config-file (local-file "etc/keepalived/vm1.wugi.info.conf"))))
                 (service gre-service-type
                          (gre-configuration
                           (ip-address-local "78.108.82.157")
                           (ip-address-remote "78.108.87.161")
                           (ip-address "10.0.0.3/24")
                           (interface-name "gre1")
                           (routes '("add 10.8.0.0/24 via 10.0.0.2"))))

                 (service slim-service-type))
           (load "desktop.scm")
           (modify-services %base-services
             (guix-service-type _ => %guix-daemon-config-with-substitute-urls)
             (sysctl-service-type _ =>
                                  (sysctl-configuration
                                   (settings (append '(("net.ipv4.ip_forward" . "1"))
                                                     %default-sysctl-settings)))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
