;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu)
             (guix modules)
             (json)
             (guix gexp))
(use-service-modules certbot databases dbus desktop docker dns messaging monitoring networking nix linux ssh sysctl web vpn)
(use-package-modules admin curl certs databases guile networking linux ssh tmux)

(use-modules (config))

(use-modules (packages certs)
             (services bird)
             (services dns)
             (services docker)
             (services mail)
             (services monitoring)
             (services certbot)
             (services kubernetes)
             (services networking)
             (services jenkins)
             (services openvpn)
             (services ssh)
             (services web))


;;;
;;; operating-system
;;;

(operating-system
  (host-name "vm1.wugi.info")
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
     (list (string-join (append '("127.0.0.1" "vm1.wugi.info" "localhost")
                                %mail-hosts-file-hosts
                                %ssh-hosts-file-hosts))
           "192.168.25.1 node-0.example.com"
           (string-join '("::1" "vm1.wugi.info" "localhost")))
     "\n")))

  (sudoers-file (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
oleg ALL=(ALL) NOPASSWD:ALL\n"))

  ;; Globally-installed packages.
  (packages (append (list curl nmap iptables mtr tcpdump net-tools iftop
                          nss-certs dn42-ca
                          strace tmux ipset)
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
                          (service iptables-service-type
                                   (iptables-configuration
                                    (ipv4-rules (local-file "etc/iptables/iptables.rules"))))
                          (service ntp-service-type
                                   (ntp-configuration
                                    (servers
                                     (list
                                      (ntp-server
                                       (type 'pool)
                                       (address "78.108.83.254")
                                       (options '("iburst")))))))
                          (service openssh-service-type
                                   (openssh-configuration
                                    (authorized-keys
                                    `(("jenkins" ,(local-file "ssh/id_rsa_jenkins.wugi.info.pub"))))
                                    (password-authentication? #f)
                                    (gateway-ports? 'client)
                                    (use-pam? #f)
                                    (extra-content "\
Match Address 127.0.0.1
PasswordAuthentication yes")))
                          (service webssh-service-type
                                   (webssh-configuration (address "127.0.0.1")
                                                         (port 8888)
                                                         (policy 'reject)
                                                         (known-hosts '("\
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIshCEWdx+lCTFsWrGbSa0hASfXXfRaqod/fVBMpbJwhrcj05ud68Ht3Zo0eGzCVBXoNnSZr02catpnjReBrOq8="
                                                                        "\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIshCEWdx+lCTFsWrGbSa0hASfXXfRaqod/fVBMpbJwhrcj05ud68Ht3Zo0eGzCVBXoNnSZr02catpnjReBrOq8="))))
                          (service wireguard-service-type
                                   (wireguard-configuration
                                    (interface "de2.g-load.eu")
                                    (addresses
                                     '("192.168.219.77/32"

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
                                                      "172.22.144.97/27"
                                                      "fe80::/10")))))))
                          (service knot-dns-service-type
                                   (knot-dns-configuration
                                    (config-file
                                     (knot-config "78.108.82.44"))))

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
                          (service openvpn-service-type
                                   (openvpn-configuration
                                    (name "wugi.info")
                                    (config (plain-file "openvpn.conf"
                                                        "\
client
proto udp
dev tapvpn1
ca /etc/openvpn/ca.crt
cert /etc/openvpn/client.crt
key /etc/openvpn/client.key
comp-lzo
persist-key
persist-tun
verb 3
nobind
ping 5
ping-restart 10
resolv-retry infinite
remote guix.wugi.info 1195
remote-random
"))))

                          (service dante-service-type
                                   (dante-configuration
                                    (config-file (local-file "sockd.conf"))
                                    (requirement '(tor))
                                    (socks-directroute-fallback? #t)))

                          (service tinyproxy-service-type
                                   (tinyproxy-configuration
                                    (config-file (local-file "tinyproxy.conf"))
                                    (requirement '(tor))))

                         (service tor-service-type
                                  (tor-configuration
                                   (config-file (local-file "torrc"))))

                         (service jenkins-builder-service-type)

                         (service nix-service-type
                                  (nix-configuration
                                   (extra-config '("trusted-users = oleg root"))))

                          (dbus-service)
                          (elogind-service)
                          (service bird-service-type
                                   (bird-configuration
                                    (config-file (local-file "bird.conf"))))

                          (service prometheus-node-exporter-service-type)
                          (service prometheus-bird-exporter-service-type
                                   (prometheus-bird-exporter-configuration
                                    (arguments '("-format.new=true"
                                                 "-bird.ipv6=false"))))
                          (service prometheus-exim-exporter-service-type
                                   (prometheus-exim-exporter-configuration
                                    (arguments '("--exim.log-path=/var/log/exim"
                                                 "--exim.input-path=/var/spool/exim/input"))))
                          (service prometheus-shepherd-exporter-service-type)

                          (service (certbot-service-type-custom-nginx "78.108.82.44")
                                   (certbot-configuration
                                    (email "go.wigust@gmail.com")
                                    (certificates
                                     (append
                                      (list
                                       (certificate-configuration
                                        (domains '("smtp.wugi.info"))
                                        (deploy-hook %exim-deploy-hook))
                                       (certificate-configuration
                                        (domains '("imap.wugi.info"))
                                        (deploy-hook %dovecot-deploy-hook)))
                                      (map (lambda (host)
                                             (certificate-configuration
                                              (domains (list host))
                                              (deploy-hook %nginx-deploy-hook)))
                                           (list "file.wugi.info"
                                                 "homer.wugi.info"
                                                 "vm1.wugi.info"
                                                 "wugi.info"
                                                 "xmpp.wugi.info"))))))

                          (service docker-service-type)

                          (service docker-compose-service-type
                                   (docker-compose-configuration
                                    (project-name "opensearch")
                                    (compose-file
                                     (computed-file
                                      "docker-compose-opensearch.json"
                                      (with-extensions (list guile-json-4)
                                        (with-imported-modules (source-module-closure '((json builder)))
                                          #~(begin
                                              (use-modules (json builder)
                                                           (ice-9 rdelim))
                                              (define filebeat-config
                                                #$(plain-file "filebeat.json"
                                                              (scm->json-string
                                                               `(("filebeat"
                                                                  ("modules" .
                                                                   #((("module" . "nginx")
                                                                      ("error"
                                                                       ("var.paths" . #("/mnt/log/nginx/error.log"))
                                                                       ("enabled" . #t))
                                                                      ("access"
                                                                       ("var.paths" . #("/mnt/log/nginx/access.log"))
                                                                       ("enabled" . #t)))
                                                                     (("syslog"
                                                                       ("var.paths" . #("/mnt/log/messages"))
                                                                       ("var.convert_timezone" . #t)
                                                                       ("enabled" . #t))
                                                                      ("module" . "system")
                                                                      ("auth"
                                                                       ("var.paths" . #("/mnt/log/secure"))
                                                                       ("enabled" . #t)))))
                                                                  ("inputs" .
                                                                   #((("type" . "log")
                                                                      ("paths" . #("/mnt/log/**/*.log"))
                                                                      ("enabled" . #t))
                                                                     (("type" . "log")
                                                                      ("paths" . #("/home/oleg/.local/var/log/*.log"))
                                                                      ("enabled" . #t))
                                                                     (("type" . "log")
                                                                      ("paths" . #("/home/oleg/.local/var/log/**/*.log"))
                                                                      ("enabled" . #t)))))
                                                                 ("output"
                                                                  ("elasticsearch"
                                                                   ("hosts" . #("https://node-0.example.com:9200"))
                                                                   ("allow_older_versions" . #t)
                                                                   ("ssl"
                                                                    ("certificate_authorities" . #("/etc/client/ca.pem"))
                                                                    ("certificate" . "/etc/client/cert.pem")
                                                                    ("key" . "/etc/client/cert.key"))))))))
                                              (with-output-to-file #$output
                                                (lambda ()
                                                  (scm->json
                                                   `(("version" . "3")
                                                     ("services"
                                                      ("filebeat"
                                                       ("volumes"
                                                        .
                                                        ,(vector (string-append filebeat-config ":/usr/share/filebeat/filebeat.yml:ro")
                                                                 "/var/log:/mnt/log:ro"
                                                                 "/home/oleg/.local/var/log:/home/oleg/.local/var/log:ro"
                                                                 "/etc/localtime:/etc/localtime:ro"
                                                                 "/etc/opensearch/root-ca.pem:/etc/client/ca.pem:ro"
                                                                 "/etc/opensearch/kirk.pem:/etc/client/cert.pem:ro"
                                                                 "/etc/opensearch/kirk-key.pem:/etc/client/cert.key:ro"))
                                                       ("image" . "docker-registry.wugi.info/monitoring/filebeat-oss:7.12.1")
                                                       ("hostname" . "vm1.wugi.info")
                                                       ("network_mode" . "host")
                                                       ("environment"
                                                        ("name" . "vm1.wugi.info"))
                                                       ("user" . "0:0")
                                                       ("command" . "filebeat -e -strict.perms=false"))))))))))))))

                          (service homer-service-type
                                   (homer-configuration
                                    (config-file %homer-wugi.info-config)
                                    (nginx
                                     (list
                                      (nginx-server-configuration
                                       (inherit %homer-wugi.info-nginx-configuration)
                                       (listen '("78.108.82.44:80 default_server"
                                                 "78.108.82.44:443 ssl default_server")))))))

                          (service prosody-service-type
                                   (prosody-configuration
                                    (virtualhosts
                                     (list
                                      (virtualhost-configuration
                                       (domain "xmpp.wugi.info"))))
                                    (ssl
                                     (ssl-configuration
                                      (key "/etc/prosody/certs/xmpp.wugi.info.key")
                                      (certificate "/etc/prosody/certs/xmpp.wugi.info.pem")))))

                          (service zram-device-service-type
                                   (zram-device-configuration
                                    (size "8G")))

                          (service yggdrasil-service-type
                                   (yggdrasil-configuration
                                    (autoconf? #f)
                                    (json-config
                                     '(("NodeInfo" . null)
                                       ("NodeInfoPrivacy" . #f)
                                       ("IfMTU" . 65535)
                                       ("IfName" . "auto")
                                       ("AllowedPublicKeys" . #())
                                       ("MulticastInterfaces" . #((("Port" . 0)
                                                                   ("Listen" . #t)
                                                                   ("Beacon" . #t)
                                                                   ("Regex" . ".*"))))
                                       ("AdminListen" . "unix:///var/run/yggdrasil.sock")
                                       ("Listen" . #())
                                       ("InterfacePeers" . null)))))

                          (service kubernetes-k3s-service-type
                                   (kubernetes-k3s-configuration
                                    (server? #t)
                                    (arguments '(;; "--node-external-ip" "201:c18b:10f7:a21:ac2c:cc10:f3e:64e6"

                                                 "--no-deploy" "traefik"
                                                 "--disable" "traefik"

                                                 "--disable-network-policy"
                                                 "--disable" "servicelb"
                                                 "--disable-kube-proxy"
                                                 "--disable-helm-controller"

                                                 "--disable" "coredns"
                                                 "--flannel-backend" "none" ;Disable Flannel entirely.

                                                 ;; use dockerd
                                                 ;; "--docker"
                                                 ;;
                                                 ;; use containerd directly
                                                 "--container-runtime-endpoint" "unix:///run/containerd/containerd.sock"
                                                 ))))

                          )

                    (%mail-services "78.108.82.44")

                    (modify-services %base-services
                      (guix-service-type config => %guix-daemon-config-with-substitute-urls)
                      (sysctl-service-type _ =>
                                           (sysctl-configuration
                                            (settings (append '(("net.ipv4.ip_forward" . "1")
                                                                ("net.ipv4.conf.all.rp_filter" . "0")
                                                                ("net.ipv4.conf.default.rp_filter" . "0"))
                                                              %default-sysctl-settings))))))))
