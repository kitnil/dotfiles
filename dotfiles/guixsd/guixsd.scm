(use-modules (gnu)
             (gnu services shepherd)
             (gnu services)
             (guix channels)
             (guix gexp)
             (guix modules)
             (guix inferior)
             (ice-9 format)
             (ice-9 rdelim)
             (json)
             (srfi srfi-1)
             (srfi srfi-26))

(use-package-modules admin audio android backup bash bittorrent curl firmware guile haskell-apps networking linux ssh suckless xdisorg xorg)

(use-service-modules admin dbus desktop docker dns mcron networking nix sound
                     xorg ssh web cgit version-control certbot
                     monitoring databases mail vpn virtualization linux sysctl)

;; Third-party modules
(use-modules (config)
             (wigust packages admin)
             (wigust packages web)
             (wigust packages linux)
             (packages certs)
             (services autofs)
             (services backup)
             (services bittorrent)
             (services docker)
             (services virtualization)
             (services nix)
             (services autossh)
             (services kresd)
             (services jenkins)
             (services monitoring)
             (services openvpn)
             (services syncthing)
             (services vnc)
             (services web)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(add-to-load-path "/home/oleg/.local/share/chezmoi/dotfiles/manifests")
(use-modules (deprecated))

(define (amdgpu+amdgpu.conf)
  (string-append "\

Section \"Device\"
        Identifier  \"amd-video-card-displayport-0\"
        Driver      \"amdgpu\"
        Option      \"TearFree\" \"true\"
        Option      \"DRI\" \"3\"
EndSection

Section \"Screen\"
   Identifier  \"Screen 1\"
   Device      \"amd-video-card-displayport-0\"
   Monitor     \"DisplayPort-0\"
EndSection
\n\n"))



;;;
;;; Certbot
;;;

(define %certbot-hosts
  (list "cgit.duckdns.org"
        "githunt.wugi.info"
        "guix.duckdns.org"
        "guix.wugi.info"
        "jenkins.wugi.info"
        "monitor.wugi.info"
        "syncthing.wugi.info"
        "torrent.wugi.info"
        "webssh.wugi.info"
        "kiwiirc.wugi.info"))


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
         (listen '("80"))
         (root "/srv/share"))
        (nginx-server-configuration
         (server-name '("netmap.intr"))
         (listen '("80"))
         (root "/home/oleg/archive/src/drawthe.net"))
        (nginx-server-configuration
         (server-name '("techinfo.intr"))
         (listen '("80"))
         (root "/var/www/techinfo.intr"))
        (nginx-server-configuration
         (server-name '("iso.wugi.info"))
         (listen '("80"))
         (root "/srv/iso"))
        (nginx-server-configuration
         (server-name '("texinfo.tld"))
         (listen '("80"))
         (root "/var/www/texinfo"))

        ;; (proxy "hms.majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "www.majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "majordomo.ru" 7777 #:ssl? #f)
        ;; (proxy "hms-dev.intr" 7777 #:ssl? #f)
        ;; (proxy "hms.majordomo.ru" 7777 #:ssl? #f)
        (nginx-server-configuration
         (server-name '("hms.majordomo.ru" "hms-dev.intr" "www.majordomo.ru" "majordomo.ru"))
         (listen '("80" "443 ssl"))
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
         (listen '("80" "443 ssl"))
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
;;          (listen '("80"))
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
         (listen '("80"))
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
         (listen '("80"))
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
         (listen '("443 ssl"))
         (ssl-certificate (letsencrypt-certificate "webssh.wugi.info"))
         (ssl-certificate-key (letsencrypt-key "webssh.wugi.info"))
         (locations
          (cons (nginx-location-configuration
                 (uri "/.well-known")
                 (body '("root /var/www;")))
                (nginx-server-configuration-locations %webssh-configuration-nginx))))

        (proxy "cups.tld" 631)
        (proxy "blog.wugi.info" 9001)
        (proxy "torrent.wugi.info" 9091 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "jenkins.wugi.info" 8090 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "syncthing.wugi.info" 8384 #:ssl? #t #:ssl-key? #t #:mtls? #t
               ;; https://docs.syncthing.net/users/faq.html#why-do-i-get-host-check-error-in-the-gui-api
               #:proxy-set-header-host "localhost")
        (proxy "monitor.wugi.info" 8080)
        (proxy "guix.duckdns.org" 5556 #:ssl? #t)
        (proxy "kiwiirc.wugi.info" 8194 #:ssl? #t #:ssl-key? #t #:mtls? #t)
        (proxy "guix.wugi.info" 5556 #:locations %nginx-lua-guix #:ssl? #t #:ssl-key? #t)
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
                       (list "443 ssl")
                       (list "80")))
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
    (target "/mnt/ssh/workstation")
    (source ":sshfs\\#ws1.wugi.info\\:/home/oleg"))
   (autofs-mount-configuration
    (target "/mnt/ssh/u226391")
    (source ":sshfs\\#web30s.majordomo.ru\\:"))
   (autofs-mount-configuration
    (target "/mnt/ssh/u7590")
    (source ":sshfs\\#web33s.majordomo.ru\\:"))
   (autofs-mount-configuration
    (target "/mnt/ssh/web30-eng")
    (source ":sshfs\\#web30.intr\\:"))
   (autofs-mount-configuration
    (target "/mnt/ssh/fedora24")
    (source ":sshfs\\#78.108.86.195\\:"))
   (autofs-mount-configuration
    (target "/mnt/ssh/debian11")
    (source ":sshfs\\#78.108.92.69\\:"))))


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
    "majordomo"
    "vm"))

(define (restic-command)
  (program-file
   "restic-command"
   #~(begin
       (use-modules (srfi srfi-1)
                    (srfi srfi-26)
                    (ice-9 rdelim))

       (setenv "RESTIC_PASSWORD"
               (string-trim-right
                (with-input-from-file "/etc/guix/secrets/restic"
                  read-string)))

       (setenv "SSL_CERT_DIR" "/run/current-system/profile/etc/ssl/certs")
       (setenv "SSL_CERT_FILE" "/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")

       (define %user-home
         (passwd:dir (getpw "oleg")))

       (define %exclude-directories
         (append '#$%root-directories
                 (map (lambda (directory)
                        (string-append %user-home "/" directory))
                      '#$%user-directories)))

       (define %backup-directories
         (list %user-home "/etc" "/root"))

       (display "Creating new Restic snapshot\n")

       (when (zero?
              (apply system*
                     (append (list (string-append #$restic "/bin/restic")
                                   "--repo" "/srv/backup/guixsd")
                             (fold (lambda (directory directories)
                                     (append (list "--exclude" directory) directories))
                                   '() %exclude-directories)
                             (list "backup")
                             %backup-directories)))
         (system* (string-append #$curl "/bin/curl")
                  "--max-time" "10"
                  "--retry" "5"
                  (string-append "https://hc-ping.com/"
                                 (string-trim-right
                                  (with-input-from-file "/etc/guix/secrets/hc-ping-backup-guix.wugi.info"
                                    read-string))))))))

(define backup-job
  ;; Run 'updatedb' at 20:00 every day.  Here we write the
  ;; job's action as a Scheme procedure.
  #~(job '(next-hour '(20))
         #$(restic-command)))


;;;
;;; Networking
;;;

;; Note: Remove vlan_mode to use tagged VLANs.
(define (create-openvswitch-bridge bridge uplink)
  #~(let ((ovs-vsctl (lambda (cmd)
                       (apply invoke/quiet
                              #$(file-append openvswitch "/bin/ovs-vsctl")
                              (string-tokenize cmd)))))
      (and (ovs-vsctl (string-append "--may-exist add-br " #$bridge))
           (ovs-vsctl (string-append "--may-exist add-port " #$bridge " "
                                     #$uplink
                                     " vlan_mode=native-untagged")))))

(define (create-openvswitch-internal-port bridge port)
  #~(invoke/quiet #$(file-append openvswitch "/bin/ovs-vsctl")
                  "--may-exist" "add-port" #$bridge #$port
                  "vlan_mode=native-untagged"
                  "--" "set" "Interface" #$port "type=internal"))

(define %openvswitch-configuration-service
  (simple-service 'openvswitch-configuration shepherd-root-service-type
                  (list (shepherd-service
                         (provision '(openvswitch-configuration))
                         (requirement '(vswitchd))
                         (start #~(lambda ()
                                    #$(create-openvswitch-bridge
                                       "br0" "enp34s0")
                                    ;; #$(create-openvswitch-internal-port
                                    ;;    "br0" "gnt0")
                                    ))
                         (respawn? #f)))))


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

      (kernel-loadable-modules (list vendor-reset-linux-module))
      (initrd microcode-initrd)
      (kernel linux-5.15)
      (firmware (cons* ;; amdgpu-firmware
                       linux-firmware %base-firmware))
      ;; (initrd-modules (append '("vfio_pci" "vfio" "vfio_iommu_type1" "vfio_virqfd")
      ;;                         %base-initrd-modules))
      (kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"

                          ;; "amd_iommu=on"
                          "iommu=pt" ;<https://wiki.archlinux.org/index.php/PCI_passthrough_via_OVMF#Setting_up_IOMMU>
                          ;; "kvm_amd.npt=1"
                          ;; "kvm_amd.avic=1"
                          ;; "pci=realloc"
                          ;; "vfio-pci.ids=1002:7340,1002:ab38"
                          ;; video=efifb:off

                          "kvm.ignore_msrs=1"
                          "vfio-pci.ids=1002:1478,1002:1479,1002:7340,1002:ab38"

                          ;; Arch Linux Forums
                          ;; Random freezes with AMD Ryzen on Linux 5.0 / Kernel & Hardware
                          ;; <https://bbs.archlinux.org/viewtopic.php?id=245608>
                          ;;
                          ;; "processor.max_cstate=5"

                          ;; [5.2/5.3][drm:amdgpu_dm_atomic_commit_tail
                          ;; [amdgpu]] *ERROR* Waiting for fences timed out or interrupted!
                          ;; (#934) · Issues · drm / amd · GitLab
                          ;; <https://gitlab.freedesktop.org/drm/amd/-/issues/934>
                          ;;
                          "amdgpu.audio=0"
                          "amdgpu.gpu_recovery=1"
                          ;; "amdgpu.lockup_timeout=1000"
                          "amdgpu.noretry=0"
                          "amdgpu.ppfeaturemask=0xfffffffb"
                          ;; "amdgpu.ppfeaturemask=0xffffffff"
                          ;; "amdgpu.dpm=0"

                          ;; Arch Linux Forums
                          ;; AMD Vega 64 GPU hang/freezes sporadically under load on recent kernels / Kernel & Hardware
                          ;; <https://bbs.archlinux.org/viewtopic.php?id=259128>
                          ;; "amdgpu.dpm=0"
                          ;; "amdgpu.aspm=0"
                          ;; "amdgpu.runpm=0"
                          ;; "amdgpu.bapm=0"

                          ;; "amdgpu.noretry=0"
                          ;; "amdgpu.dc=0"
                          ;; "amdgpu.gpu_recovery=1"
                          ))
      (packages (append (list dn42-ca ovmf)
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
                     (name "majordomo-ssh-tunnel")
                     (uid 30011)
                     (group "users")
                     (comment "SSH forwarding privilege separation user")
                     (home-directory "/home/majordomo-ssh-tunnel"))
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
         `(,(string-join '("127.0.0.1 guixsd localhost"
                           "techinfo.intr"
                           "texinfo.tld"
                           "jenkins.wugi.info"
                           "torrent.wugi.info"
                           "kiwiirc.wugi.info"
                           "syncthing.wugi.info"
                           "iso.wugi.info"
                           "cgit.duckdns.org"
                           "spb"
                           "netmap.intr"
                           ;; Majordomo
                           ;; "hms-dev.intr"
                           ;; "api-dev.intr"
                           ;; "hms-billing-dev.intr"
                           "web99.ru"
                           ))
           "::1 guixsd localhost"

           "172.16.100.60 workstation.intr"

           ;; ci.intr
           "172.16.103.82 ns1test.majordomo.ru RC-USER 33e27a01eeb1"

           "192.168.0.121 windows.local"

           "78.108.82.157 mjru"

           "192.168.100.1 r1.tld"
           "192.168.100.12 r2.tld"
           "192.168.100.120 cuirass.tld"
           "172.16.100.60 ws1.wugi.info"
           "178.250.247.125 gitlab.mjtest jenkins.mjtest"
           "130.61.95.6 oracle"
           "172.16.100.65 zdetovetskiy.intr"
           "172.16.100.61 lyashenko.intr"
           "127.0.0.1 example.com"

           "78.108.86.20 r1"
           "78.108.87.99 r2"
           "178.250.246.123 r3"
           "178.250.247.60 r4"
           "")
         "\n")))

      (services (append (list

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

                         ;; TODO: Add vendor-reset at boot
                         ;; (service kernel-module-loader-service-type
                         ;;          '("vendor-reset" "ddcci_backlight"))

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

                         ;; Desktop services
                         (service slim-service-type
                                  (slim-configuration
                                   ;; (auto-login? #t)
                                   (default-user "oleg")
                                   (gnupg? #t) ;XXX: Merge pam-gnupg in Guix repository to upstream
				   ;; (theme %slim-theme) TODO: Fix the theme.
                                   (xorg-configuration
                                    (xorg-configuration
                                     (modules (delete xf86-video-ati (delete xf86-video-nouveau (delete xf86-video-intel %default-xorg-modules))))
                                     (extra-config (list (amdgpu+amdgpu.conf)))))))
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

                         (service nix-service-type
                                  (nix-configuration
                                   (extra-config '("trusted-users = oleg "))))
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

                         (service prometheus-service-type
                                  (let ((listen-address "127.0.0.1:9090"))
                                    (prometheus-configuration
                                     (listen-address listen-address)
                                     (prometheus "/home/oleg/.nix-profile/bin/prometheus")
                                     ;; (arguments '("--log.level=debug"
                                     ;;              "--storage.tsdb.retention.time=1h"))
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
                                                                           "vm1.wugi.info:9100")))))
                                                          (scrape_interval . "5s")
                                                          (job_name . "node"))
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
                                                           #(((targets . #("127.0.0.1:6050")))))
                                                          (job_name . "restic-rest"))
                                                         ((static_configs
                                                           .
                                                           #(((targets . #("https://guix.wugi.info/"
                                                                           "https://blog.wugi.info/")))))
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
                                                          (scrape_interval . "10m")
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
                                                          (scrape_interval . "10m")
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
                                                          ("job_name" . "ssh-metrics"))))
                                                      (rule_files . #(,prometheus-alertmanager-node
                                                                      ,prometheus-alertmanager-blackbox
                                                                      ,prometheus-alertmanager-bird
                                                                      ,prometheus-alertmanager-smartctl
                                                                      ,prometheus-alertmanager-exim
                                                                      ,prometheus-alertmanager-ssh-exporter))
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
                                   (textfile-directory "/var/lib/prometheus-node-exporter")))

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
                                                      ("timeout" . "5s")
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
                                                      ("timeout" . "5s")
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
                                                                    "/dev/sdb"
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

                         (service openssh-service-type
                                  (openssh-configuration
                                   (authorized-keys
                                    `(("vm1-ssh-tunnel" ,(local-file "ssh/id_rsa_vm1.wugi.info.pub"))
                                      ("jenkins" ,(local-file "ssh/id_rsa_jenkins.wugi.info.pub"))))
                                   (x11-forwarding? #t)
                                   (gateway-ports? 'client)
                                   (password-authentication? #f)
                                   (extra-content "\
Match Address 127.0.0.1
PasswordAuthentication yes

Match Address 192.168.0.144
PasswordAuthentication yes")))

                         (service certbot-service-type
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
                                   (modules %nginx-modules)
                                   (lua-package-path %nginx-lua-package-path)
                                   (lua-package-cpath %nginx-lua-package-cpath)
                                   (server-blocks %nginx-server-blocks)))

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
                                                 (listen '("80" "443 ssl"))
                                                 (ssl-certificate (letsencrypt-certificate "cgit.duckdns.org"))
                                                 (ssl-certificate-key (letsencrypt-key "cgit.duckdns.org")))))))

                         (service tor-service-type
                                  (tor-configuration
                                   (config-file (local-file "torrc"))))

                         (service ddclient-service-type)

		         ;; TODO: Move those services.

                         #;(postgresql-service #:config-file (postgresql-config-file
                         (hba-file
                         (plain-file "pg_hba.conf"
                         "
                         local	all	all			trust
                         host	all	all	127.0.0.1/32    trust
                         host	all	all	::1/128         trust
                         host	all	all	172.16.0.0/12   trust"))
                         (extra-config '(("listen_addresses" "'0.0.0.0'")))))

                         ;; (service mongodb-service-type)

                         (service php-fpm-service-type
                                  (php-fpm-configuration
                                   (timezone "Europe/Moscow")))

                         (service jenkins-service-type %jenkins-config)

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

                         transmission-service

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
                                    ))

                         (service openvswitch-service-type)
                         %openvswitch-configuration-service
                         (service static-networking-service-type
                                  (list (static-networking
                                         (addresses
                                          (list (network-address
                                                 (device "br0")
                                                 (value "192.168.0.144/24"))
                                                (network-address
                                                 (device "enp34s0")
                                                 (value "127.0.0.2/8"))))
                                         (routes
                                          (list (network-route
                                                 (destination "default")
                                                 (gateway "192.168.0.1"))))
                                         (name-servers '("192.168.0.144"
                                                         "172.17.0.1"
                                                         "8.8.8.8"
                                                         "8.8.4.4"))
                                         (requirement '(openvswitch-configuration)))))

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
                                                        (extra-options '("--cache-failures")))))))

      (essential-services
       (modify-services (operating-system-default-essential-services this-operating-system)
         (shepherd-root-service-type config => (shepherd-configuration
					        (inherit config)
					        (shepherd shepherd-patched)))
         (sysctl-service-type _ =>
                              (sysctl-configuration
                               (settings (append '(("net.bridge.bridge-nf-call-iptables" . "0"))
                                                 %default-sysctl-settings))))))

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
