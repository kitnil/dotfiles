;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules certbot databases dbus desktop docker dns monitoring networking ssh sysctl web vpn)
(use-package-modules admin curl certs databases networking linux ssh tmux)

(use-modules (config))

(use-modules (packages certs)
             (services bird)
             (services mail)
             (services ssh))

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

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (append (list (user-account
                        (name "oleg")
                        (comment "Oleg Pykhalov")
                        (group "users")

                        ;; Adding the account to the "wheel" group
                        ;; makes it a sudoer.  Adding it to "audio"
                        ;; and "video" allows the user to play sound
                        ;; and access the webcam.
                        (supplementary-groups '("wheel" "audio" "video"))))
                 %mail-users
                 %ssh-users
                 %base-user-accounts))

  (hosts-file
   (plain-file
    "hosts"
    (string-join
     (list (string-join (append '("127.0.0.1" "vm4.wugi.info" "localhost")
                                %mail-hosts-file-hosts
                                %ssh-hosts-file-hosts))
           (string-join '("::1" "vm4.wugi.info" "localhost")))
     "\n")))

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  ;; Globally-installed packages.
  (packages (append (list curl nmap iptables mtr tcpdump net-tools iftop
                          nss-certs dn42-ca
                          strace tmux)
                    %mail-packages
                    %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append (list (static-networking-service "eth0" "78.108.82.44"
                                                     #:netmask "255.255.254.0"
                                                     #:gateway "78.108.83.254"
                                                     #:name-servers '("127.0.0.1"
                                                                      "8.8.8.8"
                                                                      "8.8.4.4"))
                          (service ntp-service-type)
                          (service openssh-service-type
                                   (openssh-configuration
                                    (password-authentication? #t)
                                    (use-pam? #f)))
                          (service wireguard-service-type
                                   (wireguard-configuration
                                    (interface "de2.g-load.eu")
                                    (addresses
                                     '("192.168.219.77/32"
                                       "172.22.144.97/27"

                                       ;; dummy address for BGP peering
                                       "172.20.53.98/27"))
                                    (peers
                                     (list
                                      (wireguard-peer
                                       (name "de2.g-load.eu")
                                       (endpoint "de2.g-load.eu:22496")
                                       (public-key "B1xSG/XTJRLd+GrWDsB06BqnIq8Xud93YVh/LYYYtUY=")
                                       (allowed-ips '("172.16.0.0/12"
                                                      "192.168.0.0/16"
                                                      ;; "172.20.53.97/32"
                                                      ;; "192.168.219.77/32"
                                                      )))))))
                          (service knot-resolver-service-type
                                   (knot-resolver-configuration
                                    (kresd-config-file (plain-file "kresd.conf" "\
net.listen('127.0.0.1')
net.ipv6 = false

modules = { 'policy' }
policy.add(policy.suffix(policy.STUB(\"172.20.0.53\"), {todname('dn42')}))

-- Forward all queries (complete stub mode)
policy.add(policy.all(policy.STUB('8.8.8.8')))

-- Smaller cache size
cache.size = 10 * MB
"))))
                          (dbus-service)
                          (elogind-service)
                          (service bird-service-type
                                   (bird-configuration
                                    (config-file (local-file "bird.conf"))))
                          (service zabbix-agent-service-type %vm-zabbix-agent-configuration)

                          (service postgresql-service-type
                                   (postgresql-configuration
                                    (config-file
                                     (postgresql-config-file
                                      (hba-file
                                       (plain-file "pg_hba.conf"
                                                   "
local	all	all			trust
host	all	all	127.0.0.1/32    trust
host	all	all	::1/128         trust
host	all	all	172.16.0.0/12   trust"))
                                      (extra-config '(("listen_addresses" "127.0.0.1")))))
                                    (postgresql postgresql-10)))

                          (service prometheus-node-exporter-service-type)

                          (service certbot-service-type
                                   (certbot-configuration
                                    (email "go.wigust@gmail.com")
                                    (certificates
                                     `(,@(map (lambda (host)
                                                (certificate-configuration
                                                 (domains (list host))
                                                 (deploy-hook %nginx-deploy-hook)))
                                              (list "zabbix.wugi.info"
                                                    "file.wugi.info"))))))

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
                                    (nginx %zabbix-nginx-configuration)))

                          (service nginx-service-type
                                   (nginx-configuration
                                    (server-blocks (list (proxy "file.wugi.info" 5091 #:ssl? #t #:ssl-key? #t)))))

                          (service docker-service-type))

                    %mail-services

                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config-with-substitute-urls)
                      (sysctl-service-type _ =>
                                           (sysctl-configuration
                                            (settings (append '(("net.ipv4.ip_forward" . "1")
                                                                ("net.ipv4.conf.all.rp_filter" . "0")
                                                                ("net.ipv4.conf.default.rp_filter" . "0"))
                                                              %default-sysctl-settings))))))))
