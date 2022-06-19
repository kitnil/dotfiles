(use-modules (gnu)
             (gnu services shepherd)
             (gnu services)
             (guix channels)
             (guix download)
             (guix gexp)
             (guix modules)
             (guix packages)
             (guix store)
             (guix inferior)
             (ice-9 format)
             (ice-9 rdelim)
             (json)
             (srfi srfi-1)
             (srfi srfi-26))

(use-package-modules admin audio android backup bash bittorrent curl dns firmware guile haskell-apps networking linux samba ssh suckless xdisorg xorg)

(use-service-modules admin dbus desktop docker dns mcron networking nix nfs sound
                     xorg ssh web cgit version-control certbot
                     monitoring databases mail vpn virtualization linux sysctl)

;; Third-party modules
(use-modules (config)
             (wigust packages admin)
             (wigust packages web)
             (wigust packages linux)
             (packages certs)
             (services admin)
             (services autofs)
             (services backup)
             (services bird)
             (services bittorrent)
             (services certbot)
             (services docker)
             (services virtualization)
             (services nix)
             (services autossh)
             (services kresd)
             (services kubernetes)
             (services jenkins)
             (services monitoring)
             (services openvpn)
             (services syncthing)
             (services vnc)
             (services web))

(define %home
  (passwd:dir (getpw "oleg")))

(add-to-load-path (string-append %home "/.local/share/chezmoi/dotfiles/manifests"))
(use-modules (deprecated))

(add-to-load-path (string-append %home "/.local/share/chezmoi/dotfiles/guixsd/modules"))
(use-modules (services networking))


;;;
;;; Certbot
;;;

(define %certbot-hosts
  (list "cgit.duckdns.org"
        "guix.duckdns.org"
        "guix.wugi.info"
        "jenkins.wugi.info"
        "monitor.wugi.info"
        "syncthing.wugi.info"
        "webssh.wugi.info"
        "peertube.wugi.info"
        "docker-registry.wugi.info"))


;;;
;;; NGINX
;;;

(define %nginx-certbot
  (nginx-location-configuration
   (uri "/.well-known")
   (body '("root /var/www;"))))

(define %nginx-server-blocks
  (list (nginx-server-configuration
         (server-name '("www.tld"))
         (listen '("192.168.0.144:80"))
         (root "/srv/share"))
        (nginx-server-configuration
         (listen '("192.168.0.144:443 ssl"))
         (ssl-certificate (letsencrypt-certificate "peertube.wugi.info"))
         (ssl-certificate-key (letsencrypt-key "peertube.wugi.info"))
         (server-name '("peertube.wugi.info"))
         (root "/var/www/peertube/storage")
         (raw-content
          (list
           (with-input-from-file (string-append
                                  %home "/.local/share/chezmoi/dotfiles/guixsd/nginx/peertube")
             read-string))))
        (nginx-server-configuration
         (server-name '("netmap.intr"))
         (listen '("192.168.0.144:80"))
         (root "/home/oleg/archive/src/drawthe.net"))
        (nginx-server-configuration
         (server-name '("techinfo.intr"))
         (listen '("192.168.0.144:80"))
         (root "/var/www/techinfo.intr"))
        (nginx-server-configuration
         (server-name '("iso.wugi.info"))
         (listen '("192.168.0.144:80"))
         (root "/srv/iso"))
        (nginx-server-configuration
         (server-name '("gitlab.wugi.info"))
         (listen '("127.0.0.1:80"))
         (locations
          (list
           (nginx-location-configuration
            (uri "/")
            (body (list
                   "resolver 80.80.80.80 ipv6=off;"
                   "proxy_pass https://gitlab.com$empty;"
                   "set $empty \"\";"
                   "proxy_set_header Host gitlab.com;"
                   "proxy_ssl_server_name on;"
                   "client_max_body_size 0;"
                   "proxy_busy_buffers_size 512k;"
                   "proxy_buffers 4 512k;"
                   "proxy_buffer_size 256k;"
                   "add_header Access-Control-Allow-Origin *;"))))))
        (nginx-server-configuration
         (server-name '("docker-registry.wugi.info"))
         (listen '("192.168.0.144:80" "192.168.0.144:443 ssl"))
         (ssl-certificate (letsencrypt-certificate "docker-registry.wugi.info"))
         (ssl-certificate-key (letsencrypt-key "docker-registry.wugi.info"))
         (locations
          (list
           (nginx-location-configuration
            (uri "/.well-known")
            (body '("root /var/www;")))
           (nginx-location-configuration
            (uri "/v2/")
            (body (list
                   ;; Do not allow connections from docker 1.5 and earlier
                   ;; docker pre-1.6.0 did not properly set the user agent on ping, catch "Go *" user agents
                   "if ($http_user_agent ~ \"^(docker\\/1\\.(3|4|5(?!\\.[0-9]-dev))|Go ).*$\" ) { return 404; }"

                   ;; from [[https://docs.docker.com/registry/recipes/nginx/][Authenticate proxy with nginx | Docker Documentation]]
                   "proxy_pass http://docker-registry;"
                   "proxy_set_header Host $http_host;"
                   "proxy_set_header X-Real-IP $remote_addr;" ;# pass on real client's IP
                   "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
                   "proxy_set_header X-Forwarded-Proto $scheme;"
                   "proxy_read_timeout 900;"

                   "client_max_body_size 0;"
                   "proxy_busy_buffers_size 512k;"
                   "proxy_buffers 4 512k;"
                   "proxy_buffer_size 256k;"
                   "add_header Access-Control-Allow-Origin *;"
                   ))))))
        (nginx-server-configuration
         (server-name '("texinfo.tld"))
         (listen '("192.168.0.144:80"))
         (root "/var/www/texinfo"))

        ;; (proxy "hms.majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "www.majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "hms-dev.intr" 7777 #:ssl? #f)
        ;; (proxy "hms.majordomo.ru" 7777 #:ssl? #f)
        (nginx-server-configuration
         (server-name '("hms.majordomo.ru" "hms-dev.intr" "www.majordomo.ru" "majordomo.ru"))
         (listen '("192.168.0.144:80" "192.168.0.144:443 ssl"))
         (ssl-certificate "/etc/tls/hms.majordomo.ru.pem")
         (ssl-certificate-key "/etc/tls/hms.majordomo.ru.key")
         (locations (list (nginx-location-configuration
                           (uri "/")
                           (body (list "root /home/oleg/majordomo/hms/frontend-app/public;"
                                       "proxy_set_header Access-Control-Allow-Origin *;"
                                       "index  index.html;"
                                       "try_files $uri $uri/ /index.html;"
                                       ;; "proxy_pass http://127.0.0.1:3000;"
                                       ;; "proxy_set_header Host hms.majordomo.ru;"
                                       ;; "proxy_set_header X-Forwarded-Proto $scheme;"
                                       ;; "proxy_set_header X-Real-IP $remote_addr;"
                                       ;; "proxy_set_header X-Forwarded-for $remote_addr;"
                                       ;; "proxy_connect_timeout 300;"
                                       ;; "client_max_body_size 0;"
                                       ))))))
        (nginx-server-configuration
         (server-name '("hms-billing-dev.intr"))
         (listen '("192.168.0.144:80" "192.168.0.144:443 ssl"))
         (ssl-certificate "/etc/tls/hms.majordomo.ru.pem")
         (ssl-certificate-key "/etc/tls/hms.majordomo.ru.key")
         ;; (root "/home/oleg/majordomo/hms/staff-frontend-app/public")
         (locations (list (nginx-location-configuration
                           (uri "/")
                           (body (list "proxy_set_header Access-Control-Allow-Origin *;"
                                       ;; "rewrite     ^   https://$server_name$request_uri?;"
                                       "root   /home/oleg/majordomo/hms/staff-frontend-app/public;"
                                       "index  index.html;"
                                       "try_files $uri $uri/ /index.html;"
                                       ;; "proxy_pass http://127.0.0.1:3001;"
                                       ;; "proxy_set_header Host hms.majordomo.ru;"
                                       ;; "proxy_set_header X-Forwarded-Proto $scheme;"
                                       ;; "proxy_set_header X-Real-IP $remote_addr;"
                                       ;; "proxy_set_header X-Forwarded-for $remote_addr;"
                                       ;; "proxy_connect_timeout 300;"
                                       ;; "client_max_body_size 0;"
                                       ;; "proxy_set_header Access-Control-Allow-Origin *;"
                                       ))))))

;;         (nginx-server-configuration
;;          (server-name '("hms-dev.intr" "hms.majordomo.ru"))
;;          (listen '("192.168.0.144:80"))
;;          (root "/home/static/hms-frontend")
;;          (raw-content (list "\
;; location / {
;;     proxy_set_header Access-Control-Allow-Origin *;
;;     root   /home/static/hms-frontend;
;;     index  index.html;
;;     try_files $uri $uri/ /index.html;
;; }
;; ")))

        (nginx-server-configuration
         (server-name '("api-dev.intr"))
         (listen '("192.168.0.144:80"))
         (raw-content (list "\
location / {
    proxy_set_header Access-Control-Allow-Origin *;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-NginX-Proxy true;
    proxy_pass http://localhost:8082/;
    proxy_ssl_session_reuse off;
    proxy_set_header Host $http_host;
    proxy_redirect off;
    proxy_buffer_size 128k;
    proxy_buffers 4 256k;
}
")))
        (nginx-server-configuration
         (server-name '("www.example.com" "example.com"))
         (listen '("192.168.0.144:80"))
         (raw-content (list "\
location / {
    proxy_set_header Access-Control-Allow-Origin *;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-NginX-Proxy true;
    proxy_pass http://localhost:8080/;
    proxy_ssl_session_reuse off;
    proxy_set_header Host $http_host;
    proxy_redirect off;
    proxy_buffer_size 128k;
    proxy_buffers 4 256k;
}
")))

        (nginx-server-configuration
         (inherit %webssh-configuration-nginx)
         (server-name '("webssh.wugi.info"))
         (listen '("192.168.0.144:443 ssl"))
         (ssl-certificate (letsencrypt-certificate "webssh.wugi.info"))
         (ssl-certificate-key (letsencrypt-key "webssh.wugi.info"))
         (locations
          (list (nginx-location-configuration
                 (uri "/.well-known")
                 (body '("root /var/www;")))
                (nginx-location-configuration
                 (uri "/")
                 (body '("proxy_pass http://127.0.0.1:8888;"
                         "proxy_http_version 1.1;"
                         "proxy_read_timeout 300;"
                         "proxy_set_header Upgrade $http_upgrade;"
                         "proxy_set_header Connection \"upgrade\";"
                         "proxy_set_header Host $http_host;"
                         "proxy_set_header X-Real-IP $remote_addr;"
                         "proxy_set_header X-Real-PORT $remote_port;"
                         "add_header Access-Control-Allow-Origin *;"))))))

        (proxy "cups.tld" 631)
        (proxy "jenkins.wugi.info" 8090 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "syncthing.wugi.info" 8384 #:ssl? #t #:ssl-key? #t #:mtls? #t
               ;; https://docs.syncthing.net/users/faq.html#why-do-i-get-host-check-error-in-the-gui-api
               #:proxy-set-header-host "localhost")
        (proxy "monitor.wugi.info" 8080)
        (proxy "guix.duckdns.org" 5556 #:ssl? #t)
        (proxy "kiwiirc.wugi.info" 8194 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "guix.wugi.info" 5556 #:ssl? #t #:ssl-key? #t)
        (proxy "pykhaloff.ddns.net" 443
               #:target "192.168.100.5"
               #:ssl? #t
               #:ssl-target? #t
               #:ssl-key? #f)
        ((lambda* (host #:key
                  (ssl? #f)
                  (ssl-target? #f)
                  (target #f)
                  (sub-domains? #f))
          (nginx-server-configuration
           (server-name (if sub-domains?
                            (list (string-append sub-domains?
                                                 (string-join (string-split host #\.)
                                                              "\\.")
                                                 "$"))
                            (list host (string-append "www." host))))
           (locations (delete #f
                              (list (nginx-location-configuration
                                     (uri "/api/queue")
                                     (body (list "return 404;")))
                                    (nginx-location-configuration
                                     (uri "/")
                                     (body (list "resolver 80.80.80.80;"
                                                 (string-append "set $target "
                                                                "ci.guix.gnu.org"
                                                                ":" (number->string 80) ";")
                                                 (format #f "proxy_pass ~a://$target;" "http")
                                                 (if sub-domains?
                                                     "proxy_set_header Host $http_host;"
                                                     (format #f "proxy_set_header Host ~a;" "ci.guix.gnu.org"))
                                                 "proxy_set_header X-Forwarded-Proto $scheme;"
                                                 "proxy_set_header X-Real-IP $remote_addr;"
                                                 "proxy_set_header X-Forwarded-for $remote_addr;"
                                                 "proxy_connect_timeout 300;"
                                                 "client_max_body_size 0;"
                                                 ;; https://qasseta.ru/q/100/368287/cloudfront-%D0%BA%D0%B0%D0%BA-%D0%BD%D0%B0%D1%81%D1%82%D1%80%D0%BE%D0%B8%D1%82%D1%8C-%D0%BE%D0%B1%D1%80%D0%B0%D1%82%D0%BD%D1%8B%D0%B9-%D0%BF%D1%80%D0%BE%D0%BA%D1%81%D0%B8-%D1%81%D0%B5%D1%80%D0%B2%D0%B5%D1%80-%D0%BD%D0%B0-%D1%81%D1%83%D1%89%D0%B5%D1%81%D1%82%D0%B2%D1%83%D1%8E%D1%89%D0%B5%D0%BC-%D0%B2%D0%B5%D0%B1-%D1%81%D0%B0%D0%B9%D1%82%D0%B5-%D0%BE%D0%B1%D1%81%D0%BB%D1%83%D0%B6%D0%B8%D0%B2%D0%B0%D1%8E%D1%89%D0%B5%D0%BC-%D1%80%D0%B0%D1%81%D0%BF%D1%80%D0%BE%D1%81%D1%82%D1%80%D0%B0%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BE%D1%82-s3
                                                 ;; "proxy_cache_bypass $http_upgrade;"
                                                 ;; "proxy_set_header Connection 'upgrade';"
                                                 ;; "proxy_set_header Upgrade $http_upgrade;"
                                                 ;; "proxy_http_version 1.1;"
                                                 ))))))
           (listen (if ssl?
                       (list "192.168.0.144:443 ssl")
                       (list "192.168.0.144:80")))
           (ssl-certificate (if ssl?
                                (letsencrypt-certificate host)
                                #f))
           (ssl-certificate-key (if ssl?
                                    (letsencrypt-key host)
                                    #f))))
         "cuirass.wugi.info")))


;;;
;;; Autofs
;;;

;; XXX: Maybe generate /etc/autofs.conf with Scheme.
;; [ autofs ]
;; timeout = 300
;; browse_mode = no
;; [ amd ]
;; dismount_interval = 300

(define %autofs-mounts
  (list
   (autofs-mount-configuration
    (target "/mnt/autofs/ssh/ws1.intr/home/oleg")
    (source ":sshfs\\#ws1.intr\\:/home/oleg"))
   (autofs-mount-configuration
    (target "/mnt/autofs/ssh/web30s.intr/home/u226391")
    (source ":sshfs\\#web30s.majordomo.ru\\:"))
   (autofs-mount-configuration
    (target "/mnt/autofs/ssh/web33s.intr/home/u7590")
    (source ":sshfs\\#web33s.majordomo.ru\\:"))
   (autofs-mount-configuration
    (target "/mnt/autofs/ssh/web30.intr/home/eng")
    (source ":sshfs\\#web30.intr\\:"))
   (autofs-mount-configuration
    (target "/mnt/autofs/ssh/web99.intr/root")
    (source ":sshfs\\#root@web99.intr\\:"))
   (autofs-mount-configuration
    (target "/mnt/autofs/ssh/web99.intr/root")
    (source ":sshfs\\#root@web99.intr\\:"))
   (autofs-mount-configuration
    (target "/mnt/autofs/ssh/web99.intr/home/oleg")
    (source ":sshfs\\#root@web99.intr\\:/home/oleg"))
   ;; TODO: Move autofs mount hierarchally lower after changing restic configuration.
   (autofs-mount-configuration
    (target "/mnt/windows/games")
    (source "://windows.local/games")
    (fstype (string-append "-fstype=cifs,ro,user=oleg,pass="
                           (string-trim-right
                            (with-input-from-file (if (string= (getenv "USER") "root")
                                                      "/etc/guix/secrets/windows"
                                                      "/dev/null")
                              read-string)))))))


;;;
;;; Backup
;;;

(define %root-directories
  '("/root/.cache"
    "/root/.guix-profile"
    "/root/.nix-profile"))

(define %user-directories
  '(".cache"
    ".guix-profile"
    ".nix-profile"
    "Downloads"
    "GNS3"
    "Videos"
    "vm"))

(define (restic-command)
  (program-file
   "restic-command"
   #~(begin
       (use-modules (srfi srfi-1)
                    (srfi srfi-26)
                    (ice-9 rdelim))

       (setenv "SSL_CERT_DIR" "/run/current-system/profile/etc/ssl/certs")
       (setenv "SSL_CERT_FILE" "/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")

       (define %user-home
         (passwd:dir (getpw "oleg")))

       (display "Creating new Restic snapshot\n")

       (when (and (let ((%backup-directories (list %user-home "/etc" "/root" "/var/lib/grafana" "/var/lib/crowdsec" "/var/lib/opensearch"))
                        (%exclude-directories
                         (append '#$%root-directories
                                 (map (lambda (directory)
                                        (string-append %user-home "/" directory))
                                      '#$%user-directories))))
                    (setenv "RESTIC_PASSWORD"
                            (string-trim-right
                             (with-input-from-file "/etc/guix/secrets/restic"
                               read-string)))

                    (zero?
                     (apply system*
                            (append (list (string-append #$restic "/bin/restic")
                                          "--repo" "/srv/backup/guixsd")
                                    (fold (lambda (directory directories)
                                            (append (list "--exclude" directory) directories))
                                          '() %exclude-directories)
                                    (list "backup")
                                    %backup-directories))))
                  (let ((%backup-directories (list "/mnt/windows/games/games/gothic/Saves"
                                                   "/mnt/windows/games/games/gothic2/Saves")))
                    (setenv "RESTIC_PASSWORD"
                            (string-trim-right
                             (with-input-from-file "/etc/guix/secrets/windows"
                               read-string)))
                    (if (and (file-exists? "/mnt/windows/games/games/gothic/Saves")
                             (file-exists? "/mnt/windows/games/games/gothic2/Saves"))
                        (zero?
                         (apply system*
                                (append (list (string-append #$restic "/bin/restic")
                                              "--repo" "/srv/backup/windows")
                                        (list "backup")
                                        %backup-directories)))
                        (begin
                          (display "Not all directories exist, skipping creating a new backup.")
                          #t))))
         (system* (string-append #$curl "/bin/curl")
                  "--max-time" "10"
                  "--retry" "5"
                  (string-append "https://hc-ping.com/"
                                 (string-trim-right
                                  (with-input-from-file "/etc/guix/secrets/hc-ping-backup-guix.wugi.info"
                                    read-string))))))))

(define backup-job
  ;; Run 'updatedb' at 17:00 every day.  Here we write the
  ;; job's action as a Scheme procedure.
  #~(job '(next-hour '(17))
         #$(restic-command)))


;;;
;;; Networking
;;;

(define (create-openvswitch-internal-port bridge port)
  #~(invoke/quiet #$(file-append openvswitch "/bin/ovs-vsctl")
                  "--may-exist" "add-port" #$bridge #$port
                  "vlan_mode=native-untagged"
                  "--" "set" "Interface" #$port "type=internal"))

;; Add interface to VLAN 154:
;;
;; <interface type='bridge'>
;;   <mac address='xx:xx:xx:xx:xx:xx'/>
;;   <source bridge='br154'/>
;;   <vlan>
;;     <tag id='154' nativeMode='untagged'/>
;;   </vlan>
;;   <virtualport type='openvswitch'>
;;     <parameters interfaceid='7c2d76d7-7c4b-4227-a117-4759b8ff994d'/>
;;   </virtualport>
;;   <target dev='netboot'/>
;;   <model type='e1000e'/>
;;   <address type='pci' domain='0x0000' bus='0x01' slot='0x01' function='0x0'/>
;; </interface>

(define %openvswitch-configuration-service
  (simple-service
   'openvswitch-configuration shepherd-root-service-type
   (list (shepherd-service
          (provision '(openvswitch-configuration))
          (requirement '(vswitchd))
          (start #~(let ((ovs-vsctl (lambda (cmd)
                                      (apply invoke/quiet
                                             #$(file-append openvswitch "/bin/ovs-vsctl")
                                             (string-tokenize cmd))))
                         (ip (lambda (cmd)
                               (apply system*
                                      #$(file-append iproute "/sbin/ip")
                                      (string-tokenize cmd))))
                         (iptables (lambda (cmd)
                               (apply system*
                                      #$(file-append iptables "/sbin/iptables")
                                      (string-tokenize cmd)))))
                     (lambda ()
                       (and (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-br" "br0")))
                            ;; XXX: Remove vlan_mode to use tagged VLANs.
                            (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-port" "br0" "enp34s0"
                                    "vlan_mode=native-untagged")))

                            ;; VLAN 154 provides:
                            ;; - Network via Whonix
                            (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-br" "br154")))
                            (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-br" "br154-vlan154" "br154" "154")))
                            (ip
                             (string-join
                              (list "link" "add" "link" "br154"
                                    "name" "br154.154"
                                    "type" "vlan"
                                    "id" "154")))
                            (iptables
                             (string-join
                              (list "-t" "nat"
                                    "-A" "POSTROUTING"
                                    "-o" "br0"
                                    "-j" "MASQUERADE")))
                            (iptables
                             (string-join
                              (list "-A" "FORWARD"
                                    "-i" "br0"
                                    "-o" "br154.154"
                                    "-m" "state"
                                    "--state" "RELATED,ESTABLISHED"
                                    "-j" "ACCEPT")))
                            (iptables
                             (string-join
                              (list "-A" "FORWARD"
                                    "-i" "br154.154"
                                    "-o" "br0"
                                    "-j" "ACCEPT")))
                            ;; OpenVPN NAT
                            (iptables
                             (string-join
                              (list "-t" "nat"
                                    "-A" "POSTROUTING"
                                    "-o" "tapvpn"
                                    "-j" "MASQUERADE")))
                            (iptables
                             (string-join
                              (list "-A" "FORWARD"
                                    "-i" "tapvpn"
                                    "-o" "br154.154"
                                    "-m" "state"
                                    "--state" "RELATED,ESTABLISHED"
                                    "-j" "ACCEPT")))
                            (iptables
                             (string-join
                              (list "-A" "FORWARD"
                                    "-i" "br154.154"
                                    "-o" "tapvpn"
                                    "-j" "ACCEPT")))
                            (iptables
                             (string-join
                              (list "-A" "FORWARD"
                                    "-i" "br0"
                                    "-o" "tapvpn"
                                    "-j" "ACCEPT")))
                            (iptables
                             (string-join
                              (list "-A" "FORWARD"
                                    "-i" "tapvpn"
                                    "-o" "br0"
                                    "-m" "state"
                                    "--state" "RELATED,ESTABLISHED"
                                    "-j" "ACCEPT")))

                            ;; access ci.guix.gnu.org via oracle on 192.168.154.2-154
                            (iptables
                             (string-join
                              (list
                               "-A" "FORWARD"
                               "-i" "br154.154"
                               "-o" "tapvpn1"
                               "-j" "ACCEPT")))
                            (iptables
                             (string-join
                              (list "-A" "FORWARD"
                                    "-i" "tapvpn1"
                                    "-o" "br154.154"
                                    "-j" "ACCEPT")))

                            ;; VLAN 155 provides:
                            ;; - DHCP with a PXE by netboot.xyz.
                            ;; - NAT for the Internet.
                            ;; - NAT for OpenVPN to a work.
                            (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-br" "br155")))
                            (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-br" "br155-vlan155" "br155" "155")))
                            ;; vlan 156
                            (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-br" "br156")))
                            (ovs-vsctl
                             (string-join
                              (list "--may-exist" "add-br" "br156-vlan156" "br156" "156")))

                            ;; VLAN 156 provides:
                            ;; - DHCP with a PXE by netboot.xyz.
                            ;; - NAT for the Internet.
                            (ip
                             (string-join
                              (list "link" "add" "link" "br156"
                                    "name" "br156.156"
                                    "type" "vlan"
                                    "id" "156")))))))
          (respawn? #f)))))

(define tftp-root
  #~(begin
      (mkdir #$output)
      (symlink (string-append #$(let ((version "2.0.56"))
                                  (origin
                                    (method url-fetch)
                                    (uri (string-append
                                          "https://github.com/netbootxyz/netboot.xyz/releases/download/"
                                          version "/netboot.xyz.efi"))
                                    (sha256
                                     (base32
                                      "1p6xs5fbyy40h89azqrq4mz7azydpkxisjiivhcz7aaqln5badc7")))))
               (string-append #$output "/netboot.xyz.efi"))))

(define %dnsmasq-service
  (simple-service
   'dnsmasq-configuration shepherd-root-service-type
   (list (shepherd-service
          (provision '(dnsmasq-configuration))
          (requirement '(vswitchd))
          (start #~(make-forkexec-constructor
                    (list #$(file-append dnsmasq "/sbin/dnsmasq")
                          "--keep-in-foreground"
                          "--pid-file=/run/dnsmasq.pid"
                          "--local-service"
                          "--cache-size=150"
                          "--dhcp-range" "192.168.154.52,192.168.154.148,12h"
                          "--dhcp-host=52:54:00:f1:75:45,192.168.154.129" ;web99
                          "--dhcp-host=52:54:00:7a:62:8d,192.168.154.130" ;nginx99
                          "--bind-interfaces"
                          "--interface=br154.154"
                          "--dhcp-boot=netboot.xyz.efi"
                          (string-append "--tftp-root="
                                         #$(run-with-store (open-connection)
                                             (gexp->derivation "tftp-root" tftp-root)))
                          "--enable-tftp"
                          "--server=192.168.0.144"
                          "--no-resolv")))
          (respawn? #f)))))

(define %dnsmasq-vlan156-service
  (simple-service
   'dnsmasq-vlan156-configuration shepherd-root-service-type
   (list (shepherd-service
          (provision '(dnsmasq-vlan156-configuration))
          (requirement '(vswitchd))
          (start #~(make-forkexec-constructor
                    (list #$(file-append dnsmasq "/sbin/dnsmasq")
                          "--keep-in-foreground"
                          "--pid-file=/run/dnsmasq-vlan156.pid"
                          "--local-service"
                          "--cache-size=150"
                          "--dhcp-range" "192.168.156.52,192.168.156.148,12h"
                          "--bind-interfaces"
                          "--interface=br156.156"
                          "--except-interface=lo"
                          "--no-resolv"
                          "--server=192.168.156.1")))
          (respawn? #f)))))

;; TODO: Add libvirtd network configuration.
;; (use-modules (sxml simple))
;; (call-with-output-string
;;   (lambda (port)
;;     (sxml->xml '(network
;;                  (portgroup (@ (name "vlan-01") (default "yes")))
;;                  (portgroup (@ (name "vlan-02"))
;;                             (vlan (tag (@ (id "2")))))
;;                  (portgroup (@ (name "vlan-03"))
;;                             (vlan (tag (@ (id "3")))))
;;                  (portgroup (@ (name "vlan-all"))
;;                             (vlan (@ (trunk "yes"))
;;                                   (tag (@ (id "2")))
;;                                   (tag (@ (id "3")))))
;;                  (virtualport (@ (type "openvswitch")))
;;                  (bridge (@ (name "ovsbr0")))
;;                  (forward (@ (mode "bridge")))
;;                  (name "ovs-network"))
;;                port)))


;;;
;;; Entryp point
;;;

(define %slim-theme
  (or (and=> (current-filename)
             (lambda (file)
               (string-append (dirname (dirname file))
                              "/fiore/modules/slim-artwork.scm")))
      "/home/oleg/src/dotfiles/fiore/modules/slim-artwork.scm"))

;; TODO: Get rid of full path
(define %hardware-file
  (or (and=> (current-filename)
             (lambda (file)
               (string-append (dirname file) "/hardware/guixsd.scm")))
      "/home/oleg/src/dotfiles/guixsd/hardware/guixsd.scm"))

(define %system-guixsd
  (let ((base-system (load %hardware-file)))
    (operating-system
      (inherit base-system)

      ;; TODO:
      ;; (bootloader (bootloader-configuration
      ;;              (inherit (operating-system-bootloader base-system))
      ;;              (menu-entries (list (menu-entry
      ;;                                   (label "netboot.xyz")
      ;;                                   (linux netboot.xyz))))))

      (file-systems (append (list (file-system
                                    (device (file-system-label "data18"))
                                    (mount-point "/srv")
                                    (options
                                     (string-join (list "compress=zstd:15"
                                                        "nossd")
                                                  ","))
                                    (type "btrfs")))
                            (map (lambda (subvolume)
                                   (file-system
                                     (device (file-system-label "btrfs1"))
                                     (mount-point (string-append "/home/oleg/" subvolume))
                                     (options (string-join (list (string-append "subvol=" subvolume)
                                                                 "compress=zstd:15"
                                                                 "ssd")
                                                           ","))
                                     (type "btrfs")))
                                 '("archive" "majordomo" "src" "Maildir"))
                            (operating-system-file-systems base-system)))

      (swap-devices '("/dev/disk/by-label/swap"))

      (kernel-loadable-modules (list vendor-reset-linux-module))

      ;; XXX: Add hardware/guixsd.scm
      ;; (initrd-modules (append '("vfio_pci" "vfio" "vfio_iommu_type1" "vfio_virqfd")
      ;;                         %base-initrd-modules))

      (packages (append (list dn42-ca ovmf cifs-utils btrfs-progs lvm2 smartmontools)
                        %my-system-packages))

      (groups (cons* (user-group (name "nixbld")
                                 (id 30100))
                     (user-group (name "adbusers"))
                     (user-group (name "docker")
                                 (system? #t))
                     (user-group (name "uinput"))
                     (user-group (name "postfix")
                                 (id 13)
                                 (system? #t))
                     (user-group (name "postdrop")
                                 (id 118)
                                 (system? #t))
                     %base-groups))

      (users (cons* (user-account
                     (name "oleg")
                     (uid 1000)
                     (comment "Oleg Pykhalov")
                     (group "users")
                     (supplementary-groups '("wheel" "adbusers" "audio" "video" "docker" "kvm" "input" "libvirt"))
                     (home-directory "/home/oleg"))
                    (user-account
                     (name "tail-ssh-tunnel")
                     (uid 30015)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/tail-ssh-tunnel"))
                    (user-account
                     (name "spb-zabbix-ssh-tunnel")
                     (uid 30020)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/spb-zabbix-ssh-tunnel"))
                    (user-account
                     (name "oracle-ssh-tunnel")
                     (uid 30021)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/oracle-ssh-tunnel"))
                    (user-account
                     (name "vm1-ssh-tunnel")
                     (uid 30022)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/vm1-ssh-tunnel"))
                    (user-account
                     (name "spb")
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/spb"))
                    (user-account
                     (name "postfix")
                     (uid 13)
                     (group "postfix")
                     (supplementary-groups '("postdrop"))
                     (comment "Postfix privilege separation user")
                     (home-directory "/opt/postfix")
                     (shell "/run/current-system/profile/sbin/nologin")
                     (system? #t))
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

      (hosts-file
       (plain-file
        "hosts"
        (string-join
         `(,(string-join '("127.0.0.1 guixsd localhost home.wugi.info gitlab.wugi.info"))
           "::1 guixsd localhost"

           ,(string-join '("192.168.0.144"
                           "guixsd"     ;for iftop hostname
                           "techinfo.intr"
                           "texinfo.tld"
                           "jenkins.wugi.info"
                           "kiwiirc.wugi.info"
                           "syncthing.wugi.info"
                           "iso.wugi.info"
                           "cgit.duckdns.org"
                           "spb"
                           "netmap.intr"
                           "vault1"
                           "peertube.wugi.info"
                           "docker-registry.wugi.info"
                           ;; Majordomo
                           ;; "hms-dev.intr"
                           ;; "api-dev.intr"
                           ;; "hms-billing-dev.intr"
                           ))

           "192.168.154.130 nginx99.intr"
           "192.168.154.129 web99.ru www.web99.ru www.web99.intr web99.intr"

           "172.16.100.60 workstation.intr"

           ;; ci.intr
           "172.16.103.82 ns1test.majordomo.ru RC-USER 33e27a01eeb1"

           "192.168.0.121 windows.local"
           "192.168.0.187 ubuntu.local"

           "78.108.82.157 mjru"

           "192.168.100.1 r1.tld"
           "192.168.100.12 r2.tld"
           "192.168.100.120 cuirass.tld"
           "172.16.100.60 ws1.wugi.info"
           "178.250.247.125 gitlab.mjtest jenkins.mjtest"
           "172.16.103.238 ci.guix.gnu.org.intr"

           "130.61.95.6 oracle"
           ,(let ((ip (string-trim-right
                       (if (= (getuid) 0)
                           (with-input-from-file "/etc/guix/secrets/oracle"
                             read-string)
                           ""))))
              (if (string-null? ip)
                  ""
                  (format #f "~a oracle.ygg" ip)))
           "192.168.25.2 oracle.ovpn"

           "172.16.100.65 zdetovetskiy.intr"
           "172.16.100.61 lyashenko.intr"
           "127.0.0.1 example.com"

           "192.168.25.2 oracle1.local irc.local"

           "78.108.86.20 r1"
           "78.108.87.99 r2"
           "178.250.246.123 r3"
           "178.250.247.60 r4"
           "")
         "\n")))

      (services (append (list

                         (service earlyoom-service-type
                                  (earlyoom-configuration
                                   (avoid-regexp "(^|/)(guix-daemon|Xorg|ssh)$")
                                   (prefer-regexp "(^|/)(chrome|firefox)$")))

                         ;; Raise the maximum number of open file descriptors
                         ;; that can be used.
                         (pam-limits-service
                          (list
                           (pam-limits-entry "*" 'both 'nofile 100000)))

                         (service crowdsec-service-type)
                         (service crowdsec-firewall-bouncer-service-type)

                         ;; (service vncserver-service-type (vncserver-configuration
                         ;;                                  (vncserver tigervnc-server)
                         ;;                                  (display 1)
                         ;;                                  (user "oleg")
                         ;;                                  (group "users")
                         ;;                                  (directory "/home/oleg")
                         ;;                                  (xstartup "/home/oleg/.vnc/xstartup-firefox")
                         ;;                                  (host-name "guixsd")))

                         (service vncserver-service-type (vncserver-configuration
                                                          (vncserver tigervnc-server)
                                                          (display 2)
                                                          (user "oleg")
                                                          (group "users")
                                                          (directory "/home/oleg")
                                                          (xstartup "/home/oleg/.xsession")
                                                          (host-name "guixsd")
                                                          (supplementary-groups
                                                           '("docker" "kvm" "libvirt" "audio" "video" "wheel" "users"))))

                         ;; (service vncserver-service-type (vncserver-configuration
                         ;;                                  (vncserver tigervnc-server-1.10.1)
                         ;;                                  (display 10)
                         ;;                                  (user "oleg")
                         ;;                                  (group "users")
                         ;;                                  (directory "/home/oleg")
                         ;;                                  (xstartup "/home/oleg/.vnc/xstartup-quassel")
                         ;;                                  (host-name "guixsd")))

                         (extra-special-file "/usr/bin/env"
                                             (file-append coreutils "/bin/env"))

                         ;; mount -t fuse and autofs
                         (extra-special-file "/bin/sshfs"
                                             (file-append sshfs "/bin/sshfs"))
                         (extra-special-file "/bin/ssh"
                                             (file-append openssh "/bin/ssh"))

                         ;; for taskexecutor
                         (extra-special-file "/bin/bash"
                                             (file-append bash "/bin/bash"))
                         ;; (extra-special-file "/bin/setquota")

                         ;; “adb” and “fastboot” without root privileges
                         (udev-rules-service 'android android-udev-rules
                                             #:groups '("adbusers"))

                         (udev-rules-service 'kvm
                                             (udev-rule
                                              "91-kvm-custom.rules"
                                              "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"\n"))

                         amdgpu-passthrough-service

                         (udev-rules-service 'wol
                                             (file->udev-rule
                                              "91-wol.rules"
                                              (mixed-text-file "91-wol.rules" ;https://wiki.archlinux.org/index.php/Wake-on-LAN
                                                               #~(string-join
                                                                  (list "ACTION==\"add\""
                                                                        "SUBSYSTEM==\"net\""
                                                                        "NAME==\"enp*\""
                                                                        (format #f "RUN+=\"~a/sbin/ethtool -s $name wol g\"~%" #$ethtool))))))

                         (udev-rules-service 'kmonad kmonad)

                         ;; (service singularity-service-type)

                         (service ladspa-service-type
                                  (ladspa-configuration (plugins (list swh-plugins))))

                         #;(service slim-service-type
                         (slim-configuration
                         (display ":1")
                         (vt "vt8")
                         (xorg-configuration
                         (xorg-configuration
                         (extra-config (list (intel+amdgpu.conf "\
                         Section \"ServerLayout\"
                         Identifier  \"Default Layout\"
                         Screen  0   \"Screen 2\"
                         Screen  1   \"Screen 1\" LeftOf \"Screen 2\"
                         EndSection")))))))

                         (service alsa-service-type)

                         ;; TODO: Fix substituters
                         ;; (service nix-service-type
                         ;;          (nix-configuration
                         ;;           (extra-config '("trusted-users = oleg root"
                         ;;                           "binary-caches = https://cache.nixos.org/ https://cache.nixos.intr/"
                         ;;                           "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.nixos.intr:6VD7bofl5zZFTEwsIDsUypprsgl7r9I+7OGY4WsubFA="
                         ;;                           "\n"))))

                         nix-service

                         (kresd-service (local-file "kresd.conf"))

                         (service openvpn-service-type %openvpn-configuration-majordomo.ru)
                         (service openvpn-service-type %openvpn-configuration-wugi.info)

                         ;; TODO:
                         ;; (openvpn-client-service
                         ;;  #:config (openvpn-client-configuration
                         ;;            ;; (dev 'tapvpn)
                         ;;            (auth-user-pass "/etc/openvpn/login.conf")
                         ;;            (remote (list
                         ;;                     ;; 78.108.80.230
                         ;;                     (openvpn-remote-configuration
                         ;;                      (name "vpn-miran.majordomo.ru"))
                         ;;                     ;; 78.108.91.250
                         ;;                     (openvpn-remote-configuration
                         ;;                      (name "vpn-dh.majordomo.ru"))
                         ;;                     ;; 81.95.28.29
                         ;;                     (openvpn-remote-configuration
                         ;;                      (name "vpn-office.majordomo.ru"))))))

                         (service autofs-service-type
                                  (autofs-configuration
                                   (mounts %autofs-mounts)))

                         (service osquery-service-type)

                         (service prometheus-service-type
                                  (let ((listen-address "127.0.0.1:9090"))
                                    (prometheus-configuration
                                     (listen-address listen-address)
                                     (prometheus "/home/oleg/.nix-profile/bin/prometheus")
                                     (arguments '(;; "--log.level=debug"
                                                  ;; "--storage.tsdb.retention.time=1h"
                                                  "--web.enable-admin-api"))
                                     (config-file
                                      (computed-file
                                       "prometheus.json"
                                       (with-extensions (list guile-json-4)
                                         (with-imported-modules (source-module-closure '((json builder)))
                                           #~(begin
                                               (use-modules (json builder))
                                               (define listen-address
                                                 #$listen-address)
                                               (define prometheus-alertmanager-node
                                                 #$(plain-file "prometheus-alertmanager-node.json"
                                                               (scm->json-string (load "node.scm"))))
                                               (define prometheus-alertmanager-blackbox
                                                 #$(plain-file "prometheus-alertmanager-blackbox.json"
                                                               (scm->json-string (load "blackbox.scm"))))
                                               (define prometheus-alertmanager-bird
                                                 #$(plain-file "prometheus-alertmanager-bird.json"
                                                               (scm->json-string (load "bird.scm"))))
                                               (define prometheus-alertmanager-smartctl
                                                 #$(plain-file "prometheus-alertmanager-smartctl.json"
                                                               (scm->json-string (load "smartctl.scm"))))
                                               (define prometheus-alertmanager-exim
                                                 #$(plain-file "prometheus-alertmanager-exim.json"
                                                               (scm->json-string (load "exim.scm"))))
                                               (define prometheus-alertmanager-ssh-exporter
                                                 #$(plain-file "prometheus-alertmanager-ssh-exporter.json"
                                                               (scm->json-string (load "ssh-exporter.scm"))))
                                               (define prometheus-alertmanager-guix
                                                 #$(plain-file "prometheus-alertmanager-guix.json"
                                                               (scm->json-string (load "alertmanager/guix.scm"))))
                                               (define http-targets
                                                 (append (list "https://wugi.info/"
                                                               "https://guix.wugi.info/"
                                                               "https://blog.wugi.info/"
                                                               ;; This endpoint provides an ‘nginx’ on jenkins.intr which proxies to
                                                               ;; ‘prixoxy’ which proxies to ‘tor’.
                                                               "http://ci.guix.gnu.org.intr")
                                                         '#$%default-substitute-urls))
                                               (with-output-to-file #$output
                                                 (lambda ()
                                                   (scm->json
                                                    `((scrape_configs
                                                       .
                                                       #(((static_configs
                                                           .
                                                           #(((targets . #(,listen-address)))))
                                                          (scrape_interval . "5s")
                                                          (job_name . "prometheus"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("127.0.0.1:9100"
                                                                           "vm1.wugi.info:9100"
                                                                           "oracle1.local:9100")))))
                                                          (scrape_interval . "5s")
                                                          (job_name . "node"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("192.168.0.1")))))
                                                          (scrape_interval . "10s")
                                                          (job_name . "tp-link")
                                                          (relabel_configs
                                                           .
                                                           #(((source_labels . #("__address__"))
                                                              (target_label . "__param_target"))
                                                             ((source_labels . #("__param_target"))
                                                              (target_label . "instance"))
                                                             ((replacement . "127.0.0.1:9101")
                                                              (target_label . "__address__")))))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("127.0.0.1:9633")))))
                                                          (scrape_interval . "10m")
                                                          (job_name . "smartctl"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("vm1.wugi.info:9324")))))
                                                          (scrape_interval . "1m")
                                                          (job_name . "bird"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("vm1.wugi.info:9636")))))
                                                          (scrape_interval . "1m")
                                                          (job_name . "exim"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("127.0.0.1:9095")))))
                                                          (honor_labels . #t)
                                                          (job_name . "pushgateway"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("127.0.0.1:9153")))))
                                                          (honor_labels . #t)
                                                          (job_name . "dnsmasq"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("127.0.0.1:6050")))))
                                                          (job_name . "restic-rest"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #(,@http-targets)))))
                                                          (scrape_interval . "30s")
                                                          (metrics_path . "/probe")
                                                          (params . ((module . #("http_2xx"))))
                                                          (relabel_configs
                                                           .
                                                           #(((source_labels . #("__address__"))
                                                              (target_label . "__param_target"))
                                                             ((source_labels . #("__param_target"))
                                                              (target_label . "instance"))
                                                             ((replacement . "127.0.0.1:9115")
                                                              (target_label . "__address__"))))
                                                          (job_name . "blackbox-http"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("192.168.0.1" ;TP-Link Archer C6
                                                                           "81.95.28.29" ;vpn-office.majordomo.ru router4.intr
                                                                           "81.95.28.27" ;vpn-office.majordomo.ru router{1,2}.intr
                                                                           "78.108.80.230" ;vpn-miran.majordomo.ru dynamic
                                                                           "78.108.87.250" ;vpn-miran.majordomo.ru static
                                                                           "78.108.91.250" ;vpn-dh.majordomo.ru
                                                                           "172.16.103.1" ;majordomo openvpn gateway
                                                                           )))))
                                                          (scrape_interval . "30s")
                                                          (metrics_path . "/probe")
                                                          (params . ((module . #("icmp"))))
                                                          (relabel_configs
                                                           .
                                                           #(((source_labels . #("__address__"))
                                                              (target_label . "__param_target"))
                                                             ((source_labels . #("__param_target"))
                                                              (target_label . "instance"))
                                                             ((replacement . "127.0.0.1:9115")
                                                              (target_label . "__address__"))))
                                                          (job_name . "blackbox-icmp"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("smtp.wugi.info:25")))))
                                                          (scrape_interval . "30s")
                                                          (metrics_path . "/probe")
                                                          (params . ((module . #("smtp_starttls"))))
                                                          (relabel_configs
                                                           .
                                                           #(((source_labels . #("__address__"))
                                                              (target_label . "__param_target"))
                                                             ((source_labels . #("__param_target"))
                                                              (target_label . "instance"))
                                                             ((replacement . "127.0.0.1:9115")
                                                              (target_label . "__address__"))))
                                                          (job_name . "blackbox-smtp-starttls"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("smtp.wugi.info:143")))))
                                                          (scrape_interval . "30s")
                                                          (metrics_path . "/probe")
                                                          (params . ((module . #("imap_starttls"))))
                                                          (relabel_configs
                                                           .
                                                           #(((source_labels . #("__address__"))
                                                              (target_label . "__param_target"))
                                                             ((source_labels . #("__param_target"))
                                                              (target_label . "instance"))
                                                             ((replacement . "127.0.0.1:9115")
                                                              (target_label . "__address__"))))
                                                          (job_name . "blackbox-imap-starttls"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("8.8.8.8")))))
                                                          (scrape_interval . "10m")
                                                          (metrics_path . "/probe")
                                                          (params . ((module . #("dns_udp_mjru_wugi_info"))))
                                                          (relabel_configs
                                                           .
                                                           #(((source_labels . #("__address__"))
                                                              (target_label . "__param_target"))
                                                             ((source_labels . #("__param_target"))
                                                              (target_label . "instance"))
                                                             ((replacement . "127.0.0.1:9115")
                                                              (target_label . "__address__"))))
                                                          (job_name . "blackbox-dns-udp-mjru-wugi-info"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("127.0.0.1:6060"))
                                                              (labels ("machine" . "guixsd")))))
                                                          (scrape_interval . "10s")
                                                          (job_name . "crowdsec_guixsd"))
                                                         (("static_configs"
                                                           .
                                                           #((("targets"
                                                               .
                                                               #("vm1.wugi.info:22"))
                                                              ("labels" ("module" . "default")))
                                                             (("targets"
                                                               .
                                                               #("web33.intr:22"))
                                                              ("labels" ("module" . "majordomo-eng")))))
                                                          ("relabel_configs"
                                                           .
                                                           #((("target_label" . "__param_target")
                                                              ("source_labels" . #("__address__")))
                                                             (("target_label" . "instance")
                                                              ("source_labels" . #("__param_target")))
                                                             (("target_label" . "__address__")
                                                              ("replacement" . "127.0.0.1:9312"))
                                                             (("target_label" . "__param_module")
                                                              ("source_labels" . #("module")))))
                                                          ("metrics_path" . "/ssh")
                                                          ("metric_relabel_configs"
                                                           .
                                                           #((("regex" . "^(module)$")
                                                              ("action" . "labeldrop"))))
                                                          ("job_name" . "ssh"))
                                                         (("static_configs"
                                                           .
                                                           #((("targets" . #("localhost:9312")))))
                                                          ("metrics_path" . "/metrics")
                                                          ("job_name" . "ssh-metrics"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("windows.local:9182")))))
                                                          (scrape_interval . "1m")
                                                          (job_name . "windows"))))
                                                      (rule_files . #(,prometheus-alertmanager-node
                                                                      ,prometheus-alertmanager-blackbox
                                                                      ,prometheus-alertmanager-bird
                                                                      ,prometheus-alertmanager-smartctl
                                                                      ,prometheus-alertmanager-exim
                                                                      ,prometheus-alertmanager-ssh-exporter
                                                                      ,prometheus-alertmanager-guix))
                                                      (global
                                                       (scrape_interval . "15s")
                                                       (external_labels
                                                        (monitor . "codelab-monitor")))
                                                      ("alerting"
                                                       ("alertmanagers"
                                                        .
                                                        #((("static_configs"
                                                            .
                                                            #((("targets"
                                                                .
                                                                #("127.0.0.1:9093"))))))))))
                                                    #:pretty #t)))))))))))

                         (service prometheus-node-exporter-service-type
                                  (prometheus-node-exporter-configuration
                                   (web-listen-address "127.0.0.1:9100")
                                   (textfile-directory "/var/lib/prometheus-node-exporter")
                                   (extra-options '("--collector.processes"))))

                         (service prometheus-tp-link-exporter-service-type
                                  (prometheus-tp-link-exporter-configuration
                                   ;; XXX: Deprecated SSH client.
                                   (ssh
                                    (begin
                                      (add-to-load-path (string-append %home "/.local/share/chezmoi/dotfiles/manifests"))
                                      (@ (deprecated) openssh)))
                                   (host "192.168.0.1")
                                   (known-hosts '("192.168.0.1 ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAAgwCyKvL9lBa+NEJhMgwWe5Fbc+Kxt8EmS4c2dZUqIPGbWWvYC9LQxrOiKWFSqenEYHyfaCpP6hj4b0s5lCmkj7FhOs2oWQYwtU/AXeWNoEFujKCZLV256tV7eetQeeWl+M4tbdlGdkTVSvxG5S6723g6zQZyc4o/3Jd/Rb5C7GBK57IN"))
                                   (listen-address "127.0.0.1:9101")
                                   (environment-variables
                                    (list
                                     (string-append
                                      "PROMETHEUS_TP_LINK_EXPORTER_PASSWORD="
                                      (string-trim-right
                                       (if (= (getuid) 0)
                                           (with-input-from-file "/etc/guix/secrets/prometheus-tp-link-exporter"
                                             read-string)
                                           "skipping /etc/guix/secrets/prometheus-tp-link-exporter")))))))

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

                         (service grafana-service-type)

                         (service prometheus-alertmanager-service-type
                                  (prometheus-alertmanager-configuration
                                   (listen-address "127.0.0.1:9093")
                                   (prometheus-alertmanager "/home/oleg/.nix-profile/bin/alertmanager")
                                   ;; (arguments '("--log.level=debug"))
                                   (config-file
                                    (computed-file
                                     "prometheus-alertmanager.json"
                                     (with-extensions (list guile-json-4)
                                       (with-imported-modules (source-module-closure '((json builder)))
                                         #~(begin
                                             (use-modules (json builder)
                                                          (ice-9 rdelim))
                                             (define password
                                               (string-trim-right
                                                #$(if (= (getuid) 0)
                                                      (with-input-from-file "/etc/prometheus-alertmanager/secrets"
                                                        read-string)
                                                      "skipping /etc/prometheus-alertmanager/secrets")))
                                             (with-output-to-file #$output
                                               (lambda ()
                                                 (scm->json
                                                  `(("global"
                                                     ("smtp_smarthost" . "smtp.wugi.info:587")
                                                     ("smtp_from" . "alertmanager@wugi.info")
                                                     ("smtp_auth_username" . "alertmanager@wugi.info")
                                                     ("smtp_auth_password" . ,password))
                                                    ("route"
                                                     ("receiver" . "smtp")
                                                     ("group_by" . #("alertname" "datacenter" "app")))
                                                    ("receivers"
                                                     .
                                                     #((("name" . "smtp")
                                                        ("email_configs" .
                                                         #((("to" . "alertmanager@wugi.info"))))))))
                                                  #:pretty #t))))))))))

                         (service prometheus-blackbox-exporter-service-type
                                  (prometheus-blackbox-exporter-configuration
                                   (listen-address "127.0.0.1:9115")
                                   (log-level "debug")
                                   (config-file
                                    (computed-file
                                     "prometheus-blackbox-exporter.json"
                                     (with-extensions (list guile-json-4)
                                       (with-imported-modules (source-module-closure '((json builder)))
                                         #~(begin
                                             (use-modules (json builder)
                                                          (ice-9 rdelim))
                                             (with-output-to-file #$output
                                               (lambda ()
                                                 (scm->json
                                                  '(("modules"
                                                     ("http_2xx"
                                                      ("timeout" . "5s")
                                                      ("prober" . "http")
                                                      ("http"
                                                       ("valid_status_codes" . #())
                                                       ("valid_http_versions" . #("HTTP/1.1" "HTTP/2" "HTTP/2.0"))
                                                       ("preferred_ip_protocol" . "ip4")
                                                       ("no_follow_redirects" . #f)))
                                                     ("icmp"
                                                      ("timeout" . "5s")
                                                      ("prober" . "icmp")
                                                      ("icmp"
                                                       ("preferred_ip_protocol" . "ip4")))
                                                     ("smtp_starttls"
                                                      ("timeout" . "20s")
                                                      ("tcp"
                                                       ("query_response"
                                                        .
                                                        #((("expect" . "^220 ([^ ]+) ESMTP (.+)$"))
                                                          (("send" . "EHLO prober\r"))
                                                          (("expect" . "^250-STARTTLS"))
                                                          (("send" . "STARTTLS\r"))
                                                          (("expect" . "^220"))
                                                          (("starttls" . #t))
                                                          (("send" . "EHLO prober\r"))
                                                          (("expect" . "^250-AUTH"))
                                                          (("send" . "QUIT\r")))))
                                                      ("prober" . "tcp"))
                                                     ("imap_starttls"
                                                      ("timeout" . "10s")
                                                      ("tcp"
                                                       ("query_response"
                                                        .
                                                        #((("expect" . "OK.*STARTTLS"))
                                                          (("send" . ". STARTTLS"))
                                                          (("expect" . "OK"))
                                                          (("starttls" . #t))
                                                          (("send" . ". capability"))
                                                          (("expect" . "CAPABILITY IMAP4rev1")))))
                                                      ("prober" . "tcp"))
                                                     ("dns_udp_mjru_wugi_info"
                                                      ("timeout" . "5s")
                                                      ("prober" . "dns")
                                                      ("dns"
                                                       ("validate_answer_rrs"
                                                        ("fail_if_not_matches_regexp"
                                                         .
                                                         #("mjru.wugi.info.\t.*\tIN\tNS\tns[1-3]*.majordomo.ru.")))
                                                       ("valid_rcodes" . #("NOERROR"))
                                                       ("query_type" . "NS")
                                                       ("query_name" . "mjru.wugi.info")))))
                                                  #:pretty #t))))))))))

                         (service karma-service-type
                                  (karma-configuration
                                   (config-file
                                    (computed-file
                                     "karma.json"
                                     (with-extensions (list guile-json-4)
                                       (with-imported-modules (source-module-closure '((json builder)))
                                         #~(begin
                                             (use-modules (json builder)
                                                          (ice-9 rdelim))
                                             (with-output-to-file #$output
                                               (lambda ()
                                                 (scm->json
                                                  `(("ui"
                                                     ("refresh" . "30s")
                                                     ("minimalGroupWidth" . 420)
                                                     ("hideFiltersWhenIdle" . #f)
                                                     ("colorTitlebar" . #f)
                                                     ("collapseGroups" . "collapsedOnMobile")
                                                     ("alertsPerGroup" . 5)
                                                     ("multiGridLabel" . "severity")
                                                     ("multiGridSortReverse" . #f))
                                                    ("silenceForm" ("strip" ("labels" . #("job"))))
                                                    ("receivers" ("strip" . #()) ("keep" . #()))
                                                    ("log" ("level" . "debug") ("config" . #f))
                                                    ("listen"
                                                     ("prefix" . "/")
                                                     ("port" . 18578)
                                                     ("address" . "127.0.0.1"))
                                                    ("labels"
                                                     ("strip" . #())
                                                     ("keep" . #())
                                                     ("color"
                                                      ("unique" . #("cluster" "instance" "@receiver"))
                                                      ("static" . #("job"))))
                                                    ("karma" ("name" . "karma-prod"))
                                                    ("debug" . #t)
                                                    ("annotations"
                                                     ("visible" . #())
                                                     ("hidden" . #("help"))
                                                     ("default" ("hidden" . #f)))
                                                    ("alertmanager"
                                                     ("servers"
                                                      .
                                                      #((("uri" . "http://127.0.0.1:9093")
                                                         ("timeout" . "10s")
                                                         ("readonly" . #f)
                                                         ("proxy" . #t)
                                                         ("name" . "guix.wugi.info"))))
                                                     ("interval" . "60s")))
                                                  #:pretty #t))))))))))

                         (service prometheus-smartctl-exporter-service-type
                                  (prometheus-smartctl-exporter-configuration
                                   (config-file
                                    (computed-file
                                     "smartctl-exporter.json"
                                     (with-extensions (list guile-json-4)
                                       (with-imported-modules (source-module-closure '((json builder)))
                                         #~(begin
                                             (use-modules (json builder)
                                                          (ice-9 rdelim))
                                             (define smartctl
                                               #$(file-append smartmontools "/sbin/smartctl"))
                                             (with-output-to-file #$output
                                               (lambda ()
                                                 (scm->json
                                                  `(("smartctl_exporter"
                                                     ("url_path" . "/metrics")
                                                     ("smartctl_location" . ,smartctl)
                                                     ("fake_json" . "no")
                                                     ("devices" . #("/dev/sda"
                                                                    ;; TODO: "/dev/sdb"
                                                                    "/dev/sdc"))
                                                     ;; TODO: Set collect_not_more_than_period to 120s after bug will be fixed.
                                                     ;; https://github.com/prometheus-community/smartctl_exporter/issues/19
                                                     ("collect_not_more_than_period" . "0s")
                                                     ("bind_to" . "127.0.0.1:9633")))
                                                  #:pretty #t))))))))))

                         (service prometheus-pushgateway-service-type
                                  (prometheus-pushgateway-configuration
                                   (listen-address "127.0.0.1:9095")
                                   (prometheus-pushgateway "/home/oleg/.nix-profile/bin/pushgateway")))

                         (service prometheus-dnsmasq-service-type
                                  (prometheus-dnsmasq-configuration
                                   (listen-address "127.0.0.1:9153")
                                   (prometheus-dnsmasq "/home/oleg/.nix-profile/bin/dnsmasq_exporter")))

                         (service openssh-service-type
                                  (openssh-configuration
                                   (authorized-keys
                                    `(("vm1-ssh-tunnel" ,(local-file "ssh/id_rsa_vm1.wugi.info.pub"))
                                      ("jenkins" ,(local-file "ssh/id_rsa_jenkins.wugi.info.pub"))
                                      ("oracle-ssh-tunnel" ,(local-file "ssh/id_rsa_oracle.pub"))
                                      ("spb-zabbix-ssh-tunnel" ,(local-file "ssh/id_rsa_spb-zabbix-ssh-tunnel.pub"))
                                      ("tail-ssh-tunnel" ,(local-file "ssh/id_rsa_tail-ssh-tunnel.pub"))))
                                   (x11-forwarding? #t)
                                   (gateway-ports? 'client)
                                   (password-authentication? #f)
                                   (use-pam? #f)
                                   (extra-content "\
Match Address 127.0.0.1
PasswordAuthentication yes

Match Address 192.168.0.144
PasswordAuthentication yes")))

                         (service nfs-service-type
                                  (nfs-configuration
                                   (exports
                                    '(("/srv"
                                       "192.168.0.0/24(ro,insecure,no_subtree_check,crossmnt,fsid=0)")))))

                         (service (certbot-service-type-custom-nginx "192.168.0.144")
                                  (certbot-configuration
                                   (email "go.wigust@gmail.com")
                                   (certificates
                                    `(,@(map (lambda (host)
                                               (certificate-configuration
                                                (domains (list host))
                                                (deploy-hook %nginx-deploy-hook)))
                                             %certbot-hosts)))))

                         (service nginx-service-type
                                  (nginx-configuration
                                   (server-blocks %nginx-server-blocks)
                                   (upstream-blocks
                                    (list
                                     (nginx-upstream-configuration
                                      (name "peertube-backend")
                                      (servers '("127.0.0.1:9001")))
                                     (nginx-upstream-configuration
                                      (name "docker-registry")
                                      (servers '("127.0.0.1:5000")))))
                                   (extra-content "\
  ## Set a variable to help us decide if we need to add the
  ## 'Docker-Distribution-Api-Version' header.
  ## The registry always sets this header.
  ## In the case of nginx performing auth, the header is unset
  ## since nginx is auth-ing before proxying.
  map $upstream_http_docker_distribution_api_version $docker_distribution_api_version {
    '' 'registry/2.0';
  }
")))

                         (service homer-service-type
                                  (homer-configuration
                                   (config-file %homer-config)
                                   (nginx
                                    (list
                                     (nginx-server-configuration
                                      (inherit %homer-nginx-configuration-nginx)
                                      (server-name '("home.wugi.info"))
                                      (locations
                                       (list (nginx-location-configuration
                                              (uri "/.well-known")
                                              (body '("root /var/www;")))
                                             (nginx-location-configuration
                                              (uri "/assets/config.yml")
                                              (body '("etag off;"
                                                      "if_modified_since off;"
                                                      ;; "add_header Last-Modified $date_gmt;"
                                                      "add_header Last-Modified \"\";")))))
                                      (listen '("127.0.0.1:80")))))))

                         (service gitolite-service-type
                                  (gitolite-configuration
                                   (admin-pubkey (local-file "/home/oleg/.ssh/id_rsa.pub"))))

                         (service cgit-service-type
                                  (cgit-configuration
                                   (branch-sort "age")
                                   (enable-commit-graph? #t)
                                   (enable-follow-links? #t)
                                   (enable-index-links? #t)
                                   (enable-log-filecount? #t)
                                   (enable-log-linecount? #t)
                                   (enable-remote-branches? #t)
                                   (enable-subject-links? #t)
                                   (remove-suffix? #t)
                                   (enable-index-owner? #f)
                                   (root-title "Personal Cgit")
                                   (snapshots (list "tar.gz"))
                                   (clone-prefix (list ;; "git://magnolia.local/~natsu"
                                                  "https://cgit.duckdns.org/git"))
                                   (nginx (list (nginx-server-configuration
                                                 (inherit %cgit-configuration-nginx)
                                                 (server-name '("cgit.duckdns.org" "git.tld"))
                                                 (locations
                                                  (append (nginx-server-configuration-locations %cgit-configuration-nginx)
                                                          (list (git-http-nginx-location-configuration
                                                                 (git-http-configuration
                                                                  (export-all? #t)))
                                                                (nginx-location-configuration
                                                                 (uri "/.well-known")
                                                                 (body '("root /var/www;"))))))
                                                 (listen '("192.168.0.144:80" "192.168.0.144:443 ssl"))
                                                 (ssl-certificate (letsencrypt-certificate "cgit.duckdns.org"))
                                                 (ssl-certificate-key (letsencrypt-key "cgit.duckdns.org")))))))

                         ;; (service tor-service-type
                         ;;          (tor-configuration
                         ;;           (config-file (local-file "torrc"))))

                         (service bird-service-type
                                  (bird-configuration
                                   (config-file (local-file "bird-wugi.info.conf"))))

                         (service ddclient-service-type)

		         ;; TODO: Move those services.

                         ;; Jun 19 17:51:38 guixsd postgres[27613]: [1-1]
                         ;; 2022-06-19 14:51:38.225 GMT [27613] FATAL:
                         ;; database files are incompatible with server
                         ;;
                         ;; Jun 19 17:51:38 guixsd postgres[27613]: [1-2]
                         ;; 2022-06-19 14:51:38.225 GMT [27613] DETAIL: The
                         ;; data directory was initialized by PostgreSQL
                         ;; version 13, which is not compatible with this
                         ;; version 14.3.
                         ;;
                         ;; (postgresql-service
;;                           #:config-file (postgresql-config-file
;;                                          (hba-file
;;                                           (plain-file "pg_hba.conf"
;;                                                       "
;; local	all	all			trust
;; host	all	all	127.0.0.1/32    trust
;; host	all	all	::1/128         trust
;; host	all	all	172.16.0.0/12   trust
;; host	all	all	192.168.64.0/20   trust"))
;;                                          (extra-config
;;                                           `(("listen_addresses"
;;                                              ,(string-join '("127.0.0.1"
;;                                                              "192.168.0.144"
;;                                                              "172.18.0.1")
;;                                                            ","))))))

                         ;; (service mongodb-service-type)

                         (service php-fpm-service-type
                                  (php-fpm-configuration
                                   (timezone "Europe/Moscow")))

                         (service jenkins-service-type %jenkins-config)

                         (service docker-compose-service-type
                                  (docker-compose-configuration
                                   (project-name "bittorrent")
                                   (compose-file
                                    (computed-file
                                     "docker-compose-bittorrent.json"
                                     (with-extensions (list guile-json-4)
                                       (with-imported-modules (source-module-closure '((json builder)))
                                         #~(begin
                                             (use-modules (json builder))
                                             (with-output-to-file #$output
                                               (lambda ()
                                                 (scm->json
                                                  `(("version" . "2.1")
                                                    ("services"
                                                     #$docker-compose-radarr-service
                                                     #$docker-compose-jackett-service))))))))))))

                         (service restic-rest-service-type
                                  (restic-rest-configuration
                                   (restic-rest "/home/oleg/.nix-profile/bin/rest-server")
                                   (listen-address "127.0.0.1:6050")
                                   (data-path "/srv/restic")
                                   (authentication? #f)
                                   (prometheus? #t)))

                         (simple-service 'my-cron-jobs
                                         mcron-service-type
                                         (list backup-job))

                         (service syncthing-service-type
                                  (syncthing-configuration (user "oleg")))

                         (service docker-service-type)
                         docker-service
                         ;; docker-kiwiirc-service

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
                                                           ("enabled" . #t))))))
                                                     ("output"
                                                      ("elasticsearch"
                                                       ("hosts" . #("https://localhost:9200"))
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
                                                     ("opensearch-node1"
                                                      ("volumes" . #("/var/lib/opensearch:/usr/share/opensearch/data"
                                                                     "/etc/opensearch:/usr/share/opensearch/config"
                                                                     ;; "/etc/opensearch/pki/root-ca.pem:/usr/share/opensearch/config/root-ca.pem:ro"
                                                                     ;; "/etc/opensearch/pki/node1.pem:/usr/share/opensearch/config/esnode.pem:ro"
                                                                     ;; "/etc/opensearch/pki/node1-key.pem:/usr/share/opensearch/config/esnode-key.pem:ro"
                                                                     ;; "/etc/opensearch/pki/admin.pem:/usr/share/opensearch/config/kirk.pem:ro"
                                                                     ;; "/etc/opensearch/pki/admin-key.pem:/usr/share/opensearch/config/kirk-key.pem:ro"
                                                                     ;; "/etc/opensearch/pki/opensearch.keystore:/usr/share/opensearch/config/opensearch.keystore:ro"
                                                                     ))
                                                      ("ulimits"
                                                       ("nofile"
                                                        ("soft" . 65536)
                                                        ("hard" . 65536))
                                                       ("memlock"
                                                        ("soft" . -1)
                                                        ("hard" . -1)))
                                                      ("ports" . #("127.0.0.1:9200:9200"
                                                                   "127.0.0.1:9600:9600"))
                                                      ("image" . "opensearchproject/opensearch:1.2.4")
                                                      ("environment" . #("cluster.name=opensearch-cluster"
                                                                         "node.name=opensearch-node1"
                                                                         "discovery.seed_hosts=opensearch-node1"
                                                                         "cluster.initial_master_nodes=opensearch-node1"
                                                                         "bootstrap.memory_lock=true"
                                                                         "OPENSEARCH_JAVA_OPTS=-Xms512m -Xmx512m"
                                                                         "compatibility.override_main_response_version=true"))
                                                      ("container_name" . "opensearch-node1"))
                                                     ("opensearch-dashboards"
                                                      ("ports" . #("127.0.0.1:5601:5601"))
                                                      ("image" . "opensearchproject/opensearch-dashboards:1.2.0")
                                                      ("expose" . #("5601"))
                                                      ("environment"
                                                       ("OPENSEARCH_HOSTS" . "[\"https://opensearch-node1:9200\"]"))
                                                      ("container_name" . "opensearch-dashboards"))
                                                     ("filebeat"
                                                      ("volumes"
                                                       .
                                                       ,(vector (string-append filebeat-config ":/usr/share/filebeat/filebeat.yml:ro")
                                                                "/var/log:/mnt/log:ro"
                                                                "/etc/localtime:/etc/localtime:ro"
                                                                "/etc/opensearch/root-ca.pem:/etc/client/ca.pem:ro"
                                                                "/etc/opensearch/kirk.pem:/etc/client/cert.pem:ro"
                                                                "/etc/opensearch/kirk-key.pem:/etc/client/cert.key:ro"))
                                                      ("image" . "oracle1.local:5000/beats/filebeat-oss:7.12.1")
                                                      ("hostname" . "guixsd")
                                                      ("network_mode" . "host")
                                                      ("environment"
                                                       ("name" . "guixsd"))
                                                      ("user" . "0:0")
                                                      ("depends_on" . #("opensearch-node1"))
                                                      ("command" . "filebeat -e -strict.perms=false"))))))))))))))

                         (service docker-compose-service-type
                                  (docker-compose-configuration
                                   (project-name "samba")
                                   (compose-file
                                    (computed-file
                                     "docker-compose-samba.json"
                                     (with-extensions (list guile-json-4)
                                       (with-imported-modules (source-module-closure '((json builder)))
                                         #~(begin
                                             (use-modules (json builder)
                                                          (ice-9 rdelim))
                                             (define password
                                               (string-trim-right
                                                #$(if (= (getuid) 0)
                                                      (with-input-from-file "/etc/guix/secrets/windows"
                                                        read-string)
                                                      "skipping /etc/guix/secrets/windows")))
                                             (with-output-to-file #$output
                                               (lambda ()
                                                 (scm->json
                                                  `(("services"
                                                     ("samba"
                                                      ("volumes" . #("/srv/lib:/public"))
                                                      ("ports"
                                                       .
                                                       #("192.168.154.1:139:139"
                                                         "192.168.154.1:445:445"))
                                                      ("image" . "dperson/samba")
                                                      ("environment"
                                                       .
                                                       #("TZ=Europe/Moscow"
                                                         "WORKGROUP=workgroup"
                                                         "USERID=1000"
                                                         "GROUPID=998"))
                                                      ("container_name" . "samba")
                                                      ("command"
                                                       .
                                                       ,(string-append
                                                         "-u \"oleg;"
                                                         password
                                                         "\" -s \"media;/share;yes;no;no;workgroup\" -s \"public;/public;yes;no;yes\""
                                                         " -g \"acl allow execute always = True\"")))))))))))))))

                         (service kubernetes-k3s-service-type
                          (kubernetes-k3s-configuration
                           (arguments '("--node-external-ip" "192.168.0.145"
                                        "--bind-address" "192.168.0.145"

                                        "--no-deploy" "traefik"
                                        "--disable" "traefik"

                                        "--docker"

                                        ;; "--container-runtime-endpoint" "/run/containerd/containerd.sock"
                                        ))
                           (kubevirt? #t)))

                         kubernetes-k3s-service

                         (dovecot-service
                          #:config (dovecot-configuration
                                    (listen '("127.0.0.1"))
                                    (disable-plaintext-auth? #f)
                                    (mail-location
                                     (string-append "maildir:~/Maildir"
                                                    ":INBOX=~/Maildir/INBOX"
                                                    ":LAYOUT=fs"))))

                         (service guix-publish-service-type
                                  (guix-publish-configuration
                                   (host "0.0.0.0")
                                   (port 5556)
                                   (ttl (* 90 24 3600))))

                         (service (@ (services autossh) autossh-service-type)
                                  ((@ (services autossh) autossh-configuration)
                                   (autossh-client-config
                                    (autossh-client-configuration
                                     (hosts (list (autossh-client-host-configuration
                                                   (host "znc.wugi.info")
                                                   (identity-file "/etc/autossh/id_rsa_oracle")
                                                   (strict-host-key-checking? #f)
                                                   (user "opc")
                                                   (user-known-hosts-file "/dev/null")
                                                   (extra-options
                                                    "
LocalForward 0.0.0.0:8060 127.0.0.1:8060
LocalForward 0.0.0.0:6667 127.0.0.1:6667
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                                   (host "znc.wugi.info")))

                         (service webssh-service-type
                                  (webssh-configuration (address "127.0.0.1")
                                                        (port 8888)
                                                        (policy 'reject)
                                                        (known-hosts '("\
localhost ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOnaDeOzwmrcrq1D8slYaeFozXZ0cpqNU0EvGmgnO29aiKkSD1ehbIV4vSxk3IDXz9ClMVPc1bTUTrYhEVHdCks="
                                                                       "\
127.0.0.1 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOnaDeOzwmrcrq1D8slYaeFozXZ0cpqNU0EvGmgnO29aiKkSD1ehbIV4vSxk3IDXz9ClMVPc1bTUTrYhEVHdCks="))))

                         (service kernel-module-loader-service-type
                                  '("vfio-pci" ;GPU passthrough
                                    "vendor-reset" ;reset NAVI10 (5500XT)
                                    "dm-snapshot"
                                    ;; "ddcci_backlight"
                                    ))

                         (service vault-service-type
                                  (vault-configuration
                                   (config-file
                                    (computed-file
                                     "vault.json"
                                     (with-extensions (list guile-json-4)
                                       (with-imported-modules (source-module-closure '((json builder)))
                                         #~(begin
                                             (use-modules (json builder))
                                             (with-output-to-file #$output
                                               (lambda ()
                                                 (scm->json
                                                  '(("ui" . #t)
                                                    ("telemetry"
                                                     .
                                                     #((("prometheus_retention_time" . "30s")
                                                        ("disable_hostname" . #t))))
                                                    ("storage"
                                                     ("raft"
                                                      ;; ("retry_join"
                                                      ;;  .
                                                      ;;  #((("leader_api_addr" . "http://vault2:8220"))
                                                      ;;    (("leader_api_addr" . "http://vault3:8230"))))
                                                      ("path" . "/var/lib/vault/data")
                                                      ("node_id" . "vault1")))
                                                    ("listener"
                                                     ("tcp"
                                                      ("tls_disable" . #t)
                                                      ("telemetry"
                                                       ("unauthenticated_metrics_access" . #t))
                                                      ("address" . "127.0.0.1:8210")))
                                                    ("disable_mlock" . #t)
                                                    ("cluster_addr" . "http://vault1:8211")
                                                    ("api_addr" . "http://vault1:8210")))))))))

                                    )))

                         (service openvswitch-service-type)
                         %openvswitch-configuration-service
                         (service static-networking-service-type
                                  (list (static-networking
                                         (addresses
                                          (list (network-address
                                                 (device "br0")
                                                 (value "192.168.0.144/24"))
                                                (network-address
                                                 (device "br0")
                                                 (value "192.168.0.145/24"))
                                                (network-address
                                                 (device "enp34s0")
                                                 (value "127.0.0.2/8"))
                                                (network-address
                                                 (device "br154")
                                                 (value "127.0.0.3/8"))

                                                ;; assign an ip address to bring interface up at boot, so
                                                ;; it could be used in a
                                                ;; docker network
                                                (network-address
                                                 (device "br155-vlan155")
                                                 (value "127.0.0.4/8"))

                                                (network-address
                                                 (device "br154.154")
                                                 (value "192.168.154.1/24"))

                                                ;; dummy ip to bring interface up
                                                (network-address
                                                 (device "br156")
                                                 (value "127.0.0.156/8"))
                                                (network-address
                                                 (device "br156.156")
                                                 (value "192.168.156.1/24"))))
                                         (routes
                                          (list (network-route
                                                 (destination "default")
                                                 (gateway "192.168.0.1"))))
                                         (name-servers '("192.168.0.144"

                                                         ;; local Docker
                                                         ;; "172.17.0.1"

                                                         ;; Google
                                                         ;; "8.8.8.8"
                                                         ;; "8.8.4.4"
                                                         ))
                                         (requirement '(openvswitch-configuration)))))

                         %dnsmasq-service
                         ;; TODO: Use system service after adding all required flags.
                         ;; (service dnsmasq-service-type
                         ;;          (dnsmasq-configuration
                         ;;           (listen-addresses '("192.168.154.1"))
                         ;;           ;; TODO: Replace port with --bind-interfaces
                         ;;           (port 0)))

                         %dnsmasq-vlan156-service

                         (service libvirt-service-type)
                         (simple-service 'libvirt-qemu-config activation-service-type
                                         #~(begin
                                             (when (file-exists? "/etc/libvirt")
                                               (with-output-to-file "/etc/libvirt/qemu.conf"
                                                 (lambda ()
                                                   (display "\
user = \"oleg\"

nvram = [
  \"/home/oleg/.nix-profile/FV/OVMF_CODE.fd:/home/oleg/.nix-profile/FV/OVMF_VARS.fd\"
]

namespaces = [ ]
"))))))

                         (service virtlog-service-type
                                  (virtlog-configuration
                                   (max-clients 1000)))

                         (bluetooth-service #:auto-enable? #t))

                        (load "desktop.scm")

                        (modify-services (operating-system-user-services base-system)
                          (guix-service-type config => (guix-configuration
                                                        (inherit %guix-daemon-config)
                                                        (extra-options '("--cache-failures"))))
                          (sysctl-service-type _ =>
                                               (sysctl-configuration
                                                (settings (append '(("net.bridge.bridge-nf-call-iptables" . "0")
                                                                    ;; opensearch requirement
                                                                    ("vm.max_map_count" . "262144"))
                                                                  %default-sysctl-settings)))))))

      (setuid-programs %my-setuid-programs)

      (sudoers-file (plain-file "sudoers"
                                (string-join `("root ALL=(ALL) ALL"
                                               "%wheel ALL=(ALL) ALL"
                                               "oleg ALL=(ALL) NOPASSWD:ALL"
                                               ,(format #f "majordomo-ssh-tunnel ALL=(root) NOPASSWD: ~a~%"
                                                        (string-join '("/run/current-system/profile/bin/herd * vncserver2"
                                                                       "/run/current-system/profile/bin/herd * vncserver10")
                                                                     ",")))
                                             "\n"))))))

%system-guixsd
