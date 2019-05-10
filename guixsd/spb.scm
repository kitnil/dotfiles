;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu) (srfi srfi-1) (srfi srfi-26))
(use-package-modules admin base bash certs python)
(use-service-modules monitoring networking ssh web)
(use-modules (services autossh)
             (services docker))


;;;
;;; NGINX
;;;

;; TODO: Create /var/cache/nginx/nar directory

(define guix-nginx-configuration
  (nginx-configuration
   (extra-content (format #f "~{    ~a~%~}"
                          '("# cache for nar files"
                            "proxy_cache_path /var/cache/nginx/nar"
                            "levels=2"
                            "inactive=8d	   # inactive keys removed after 8d"
                            "keys_zone=nar:4m      # nar cache meta data: ~32K keys"
                            "max_size=10g;         # total cache data size max"

                            "# cache for content-addressed files"
                            "proxy_cache_path /var/cache/nginx/cas"
                            "levels=2"
                            "inactive=180d	   # inactive keys removed after 180d"
                            "keys_zone=cas:8m      # nar cache meta data: ~64K keys"
                            "max_size=50g;         # total cache data size max"

                            "# cache for build logs"
                            "proxy_cache_path /var/cache/nginx/logs"
                            "levels=2"
                            "inactive=60d          # inactive keys removed after 60d"
                            "keys_zone=logs:8m     # narinfo meta data: ~64K keys"
                            "max_size=4g;          # total cache data size max"

                            "# cache for static data"
                            "proxy_cache_path /var/cache/nginx/static"
                            "levels=1"
                            "inactive=10d	   # inactive keys removed after 10d"
                            "keys_zone=static:1m   # nar cache meta data: ~8K keys"
                            "max_size=200m;        # total cache data size max"

                            "# If Hydra cannot honor these delays, then something is wrong and"
                            "# we'd better drop the connection and return 504."
                            "proxy_connect_timeout 7s;"
                            "proxy_read_timeout 10s;"
                            "proxy_send_timeout 10s;"

                            "# Cache timeouts for a little while to avoid increasing pressure."
                            "proxy_cache_valid 504 30s;")))
   (server-blocks
    (list
     (nginx-server-configuration
      (server-name '("cuirass.tld"))
      (listen '("80"))
      (ssl-certificate #f)
      (ssl-certificate-key #f)
      (locations (list (nginx-location-configuration
                        (uri "/")
                        (body '("# Cuirass"
                                "resolver 80.80.80.80;"
                                "proxy_pass http://localhost:8081;")))

                       (nginx-location-configuration
                        (uri "= /nix-cache-info")
                        (body '("proxy_pass http://localhost:3000/nix-cache-info;"

                                "resolver 80.80.80.80;"

                                "# Cache this file since that's always the first thing we ask for."
                                "proxy_cache static;"

                                "# Cache hits for a looong time."
                                "proxy_cache_valid 200 100d;"

                                "# Cache misses/others for 5 min."
                                "proxy_cache_valid any 5m;"

                                "proxy_ignore_client_abort on;"

                                "# We need to hide and ignore the Set-Cookie header to enable caching."
                                "proxy_hide_header    Set-Cookie;"
                                "proxy_ignore_headers Set-Cookie;")))

                       (nginx-location-configuration
                        (uri "/nar/")
                        (body '("proxy_pass http://localhost:3000;"

                                "resolver 80.80.80.80;"

                                "client_body_buffer_size 256k;"

                                "# Be more tolerant of delays when fetching a nar."
                                "proxy_read_timeout 60s;"
                                "proxy_send_timeout 60s;"

                                "# Enable caching for nar files, to avoid reconstructing and recompressing"
                                "# archives."
                                "proxy_cache nar;"
                                "proxy_cache_valid 200 30d;   # cache hits for 1 month"
                                "proxy_cache_valid 504 3m;    # timeout, when hydra.gnu.org is overloaded"
                                "proxy_cache_valid any 1h;    # cache misses/others for 1h."

                                "proxy_ignore_client_abort on;"

                                "# Nars are already compressed."
                                "gzip off;"

                                "# We need to hide and ignore the Set-Cookie header"
                                "# to enable caching."
                                "proxy_hide_header    Set-Cookie;"
                                "proxy_ignore_headers Set-Cookie;"

                                "# Provide a 'content-length' header so that 'guix substitute-binary"
                                "# knows upfront how much it is downloading."
                                "#add_header Content-Length $body_bytes_sent;")))

                       (nginx-location-configuration
                        (uri "~ \\.narinfo$")
                        (body '("# Since 'guix publish' has its own caching, and since it relies on"
                                "# the atime of cached narinfos to determine whether a narinfo can"
                                "# be removed from the cache, don't do any caching here."
                                "proxy_pass http://localhost:3000;"

                                "resolver 80.80.80.80;"

                                "# For HTTP pipelining.  This has a dramatic impact on performance."
                                "client_body_buffer_size 128k;"

                                "# Narinfos requests are short, serve many of them on a connection."
                                "keepalive_requests 600;"

                                "# Do not tolerate slowness of hydra.gnu.org when fetching"
                                "# narinfos: better return 504 quickly than wait forever."
                                "proxy_connect_timeout 2s;"
                                "proxy_read_timeout 2s;"
                                "proxy_send_timeout 2s;"

                                "# 'guix publish --ttl' produces a 'Cache-Control' header for use"
                                "# by 'guix substitute'.  Let it through rather than use nginx's"
                                "# “expire” directive since the expiration time defined by 'guix"
                                "# publish' is the right one."
                                "proxy_pass_header Cache-Control;"

                                "proxy_ignore_client_abort on;"

                                "# We need to hide and ignore the Set-Cookie header"
                                "# to enable caching."
                                "proxy_hide_header    Set-Cookie;"
                                "proxy_ignore_headers Set-Cookie;")))

                       (nginx-location-configuration
                        (uri "/log/")
                        (body '("proxy_pass http://localhost:3000;"

                                "resolver 80.80.80.80;"

                                "# Enable caching for build logs."
                                "proxy_cache logs;"
                                "proxy_cache_valid 200 60d;   # cache hits."
                                "proxy_cache_valid 504 3m;    # timeout, when hydra.gnu.org is overloaded"
                                "proxy_cache_valid any 1h;    # cache misses/others."

                                "proxy_ignore_client_abort on;"

                                "# We need to hide and ignore the Set-Cookie header"
                                "# to enable caching."
                                "proxy_hide_header    Set-Cookie;"
                                "proxy_ignore_headers Set-Cookie;")))

                       (nginx-location-configuration
                        (uri "/file/")
                        (body '("# Content-addressed files served by 'guix publish'."
                                "proxy_pass http://localhost:3000;"

                                "resolver 80.80.80.80;"

                                "proxy_cache cas;"
                                "proxy_cache_valid 200 200d;	# cache hits"
                                "proxy_cache_valid any 5m;	# cache misses/others"

                                "proxy_ignore_client_abort on;"))))))))))

;;;
;;; Entry point
;;;

(operating-system
  (host-name "spb")
  (timezone "Europe/Moscow")
  (locale "en_US.utf8")

 (initrd-modules (append (list "mptspi")
                                %base-initrd-modules))

  ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
  ;; target hard disk, and "my-root" is the label of the target
  ;; root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/sda")))
  (file-systems (cons (file-system
                        (device (file-system-label "spb-root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (groups (cons (user-group (name "nixbld")
                            (id 30100))
                (user-group (name "docker")
                            (system? #t))
                %base-groups))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons* (user-account
                 (name "oleg")
                 (comment "Oleg Pykhalov")
                 (group "users")

                 ;; Adding the account to the "wheel" group
                 ;; makes it a sudoer.  Adding it to "audio"
                 ;; and "video" allows the user to play sound
                 ;; and access the webcam.
                 (supplementary-groups '("wheel" "audio" "video" "docker"))
                 (home-directory "/home/oleg"))
                (user-account
                 (name "jenkins")
                 (group "users")
                 (uid 30018)
                 (comment "Jenkins privilege separation user")
                 (home-directory "/home/jenkins"))
                (user-account
                 (name "gitlab-runner")
                 (group "users")
                 (uid 30019)
                 (comment "GitLab Runner privilege separation user")
                 (home-directory "/home/gitlab-runner"))
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

  ;; Globally-installed packages.
  (packages (cons nss-certs ;SSL certificates
                  %base-packages))

  (hosts-file
   (plain-file "hosts"
               (string-append
                (local-host-aliases host-name)
                "\n\n"
                "192.168.125.16 cuirass.tld input.tld\n")))

  (services (cons* (service openssh-service-type)
                   (static-networking-service "eno16777736"
                                              "192.168.125.16"
                                              #:netmask "255.255.255.0" 
                                              #:gateway "192.168.125.254"
                                              #:name-servers '("80.80.80.80" "80.80.81.81"))

                          (service autossh-service-type
                            (autossh-configuration
                             (openssh-client-config
                              (openssh-client-configuration
                               (hosts (list (openssh-client-host-configuration
                                             (host "guix.duckdns.org")
                                             (identity-file "/etc/autossh/id_rsa")
                                             (strict-host-key-checking? #f)
                                             (user "tail-ssh-tunnel")
                                             (user-known-hosts-file "/dev/null")
                                             (extra-options
                                              "
RemoteForward 0.0.0.0:19022 127.0.0.1:22
RemoteForward 0.0.0.0:19050 127.0.0.1:10050
RemoteForward 0.0.0.0:19080 127.0.0.1:80
RemoteForward 0.0.0.0:19081 127.0.0.1:8081
RemoteForward 0.0.0.0:19300 127.0.0.1:3000
LocalForward 0.0.0.0:8086 127.0.0.1:8086
Compression yes
ExitOnForwardFailure yes
ServerAliveInterval 30
ServerAliveCountMax 3"))))))
                             (host "guix.duckdns.org")))

                          (service guix-publish-service-type
                                   (guix-publish-configuration
                                    (host "0.0.0.0") (port 3000)))

                          (service nginx-service-type
                                   guix-nginx-configuration)

                          (service zabbix-agent-service-type)

                          docker-service

                   %base-services)))
