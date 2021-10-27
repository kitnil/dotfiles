;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu))
(use-service-modules certbot networking mail monitoring ssh sysctl web)
(use-package-modules admin certs mail screen ssh linux)

(use-modules (config)
             (packages mail)
             (services homer)
             (services keepalived)
             (services networking)
             (services openvpn))

(define %exim-deploy-hook
  (program-file
   "exim-deploy-hook"
   ;; XXX: Send SIGHUP to exim.
   #~(begin
       (unless (file-exists? "/etc/exim")
         (mkdir "/etc/exim"))
       (let* ((cert-directory (getenv "RENEWED_LINEAGE"))
              (user (getpw "exim"))
              (uid (passwd:uid user))
              (gid (passwd:gid user)))
         (copy-file (string-append cert-directory "/"
                                   (readlink (string-append cert-directory "/fullchain.pem")))
                    "/etc/exim/exim.crt")
         (copy-file (string-append cert-directory "/"
                                   (readlink (string-append cert-directory "/privkey.pem")))
                    "/etc/exim/exim.pem")
         (chown "/etc/exim/exim" uid gid)
         (chown "/etc/exim/exim.crt" uid gid)
         (chown "/etc/exim/exim.pem" uid gid)))))

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

  (groups (append (list (user-group
                         (name "alertmanager")
                         (system? #t)))
                  %base-groups))

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
                        (supplementary-groups '("wheel"
                                                "audio" "video")))

                       (user-account
                        (name "wigust")
                        (comment "Oleg Pykhalov")
                        (group "users")
                        (supplementary-groups '("audio" "video")))

                       (user-account
                        (name "alertmanager")
                        (group "alertmanager")
                        (system? #t)
                        (comment "prometheus-alertmanager privilege separation user")
                        (shell #~(string-append #$shadow "/sbin/nologin"))))
                 %base-user-accounts))

  (hosts-file
   (plain-file
    "hosts"
    (string-join
     `(,(string-join '("127.0.0.1 guixsd localhost wugi.info"))
       "::1 guixsd localhost"
       "")
     "\n")))

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  ;; Globally-installed packages.
  (packages (cons* dovecot screen nss-certs swaks strace binutils %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append (list (static-networking-service "eth0" "78.108.87.161"
                                                     #:netmask "255.255.254.0"
                                                     #:gateway "78.108.87.254"
                                                     #:name-servers '("8.8.8.8" "8.8.4.4"))
                          (service ntp-service-type)
                          (service zabbix-agent-service-type %vm-zabbix-agent-configuration)
                          (service prometheus-node-exporter-service-type)
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
                                    (interface-name "gre1")
                                    (routes '("add 10.9.0.0/24 via 10.0.0.3"))))
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
port 1194
server 10.8.0.0 255.255.255.0
dh /etc/openvpn/dh2048.pem
ifconfig-pool-persist /etc/openvpn/ipp.txt
client-to-client
keepalive 5 10
max-clients 100
status /var/run/openvpn/status
push \"route 10.0.0.0 255.255.255.0\"
"))))

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
                                                                         (nginx-server-configuration-locations %webssh-configuration-nginx)))))
                                                         %githunt-nginx-configuration
                                                         %homer-nginx-configuration))))

                          (service homer-service-type %homer-config)
                          (service webssh-service-type
                                   (webssh-configuration (address "127.0.0.1")
                                                         (port 8888)
                                                         (policy 'reject)
                                                         (known-hosts '("\
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHVSCVdQEHUaTnBqA2nKQXRmo/74DgnyCyWiOI/f5G7qYUMfDiJqYHqh7YngyxIG9iakEUOaNtr6ljHyBXhlaPQ="
                                                                        "\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHVSCVdQEHUaTnBqA2nKQXRmo/74DgnyCyWiOI/f5G7qYUMfDiJqYHqh7YngyxIG9iakEUOaNtr6ljHyBXhlaPQ="))))

                          (service mail-aliases-service-type '(("wigust" "oleg")
                                                               ("admin" "oleg")
                                                               ("alertmanager" "oleg")))
                          (service exim-service-type
                                   (exim-configuration
                                    (package exim-lmtp)
                                    (config-file (local-file "exim.conf"))))
                          (dovecot-service
                           #:config (dovecot-configuration
                                     (disable-plaintext-auth? #f)
                                     (protocols
                                      (list (protocol-configuration (name "imap"))
                                            (protocol-configuration (name "lmtp"))))
                                     (auth-username-format "%n")
                                     (mail-location
                                      (string-append "maildir:~/Maildir"
                                                     ":INBOX=~/Maildir/INBOX"
                                                     ":LAYOUT=fs"))
                                     (services
                                      (list
                                       (service-configuration
                                        (kind "auth")
                                        (listeners
                                         (list
                                          (unix-listener-configuration
                                           (group "exim")
                                           (mode "0660")
                                           (path "auth-client"))))
                                        (process-limit 1))
                                       (service-configuration
                                        (kind "auth")
                                        (service-count 0)
                                        (client-limit 10)
                                        (process-limit 1)
                                        (listeners
                                         (list (unix-listener-configuration (path "auth-userdb")))))))))
                          (service certbot-service-type
                                   (certbot-configuration
                                    (email "admin@wugi.info")
                                    (certificates
                                     (list
                                      (certificate-configuration
                                       ;; TODO:
                                       ;; mkdir /etc/exim
                                       ;; cp /etc/letsencrypt/archive/smtp.wugi.info/fullchain1.pem /etc/exim/exim.crt
                                       ;; cp /etc/letsencrypt/archive/smtp.wugi.info/privkey1.pem /etc/exim/exim.pem
                                       ;; chown exim: -R /etc/exim
                                       (domains '("smtp.wugi.info"))
                                       (deploy-hook %exim-deploy-hook)))))))
                    (modify-services %base-services
                      (guix-service-type _ => %guix-daemon-config-with-substitute-urls)
                      (sysctl-service-type _ =>
                                           (sysctl-configuration
                                            (settings (append '(("net.ipv4.ip_forward" . "1"))
                                                              %default-sysctl-settings))))) )))
