;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system image -t docker --network ./docker-image-workstation.scm
;; skopeo copy docker-archive:/gnu/store/…-tor-docker-pack.tar.gz docker-daemon:example.org:5000/tor:latest --insecure-policy
;; docker run --network=host --security-opt seccomp=unconfined --detach --name tor --network=host example.org:5000/tor
;; docker exec --detach tor /gnu/store/…-tor-0.4.6.10/bin/tor -f /gnu/store/…-torrc

(define-module (wugi system docker-image)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system linux-container)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels docker-image)
  #:export (%docker-image))

(define (%docker-image)
  (operating-system
    (host-name "workstation")
    (timezone "Europe/Moscow")
    (locale "en_US.utf8")

    ;; This is where user accounts are specified.  The "root" account is
    ;; implicit, and is initially created with the empty password.
    (users (append (list (user-account
                           (name "oleg")
                           (comment "Oleg Pykhalov")
                           (group "users")
                           (supplementary-groups '("wheel"
                                                   "audio"
                                                   "video"
                                                   "kvm"
                                                   "input"))
                           (password (crypt "oleg" "NmhJoj")))
                         (user-account (inherit %root-account)
                                       (password (crypt "root" "uUxBgD"))))
                   %base-user-accounts))

    ;; Globally-installed packages.
    (packages (append (list bash-completion openssh)
                      %base-packages))

    ;; Because the system will run in a Docker container, we may omit many
    ;; things that would normally be required in an operating system
    ;; configuration file.  These things include:
    ;;
    ;;   * bootloader
    ;;   * file-systems
    ;;   * services such as mingetty, udevd, slim, networking, dhcp
    ;;
    ;; Either these things are simply not required, or Docker provides
    ;; similar services for us.

    ;; This will be ignored.
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("does-not-matter"))))

    ;; This will be ignored, too.
    (file-systems (list (file-system
                          (device "does-not-matter")
                          (mount-point "/")
                          (type "does-not-matter"))))

    ;; Guix is all you need!
    (services
     (append
      (list
       (service nginx-service-type
                (nginx-configuration
                  (server-blocks
                   (list
                    (nginx-server-configuration
                      (server-name '("guix.localhost"))
                      (listen '("*:80"))
                      (locations
                       (list
                        (nginx-location-configuration
                          (uri "/")
                          (body
                           '("resolver 80.80.80.80 ipv6=off;"
                             "proxy_pass https://mirrors.sjtug.sjtu.edu.cn/guix/;"
                             "proxy_set_header Host mirrors.sjtug.sjtu.edu.cn;"
                             "proxy_ssl_server_name on;"
                             "client_max_body_size 0;"
                             "proxy_busy_buffers_size 512k;"
                             "proxy_buffers 4 512k;"
                             "proxy_buffer_size 256k;"
                             "add_header Access-Control-Allow-Origin *;"))))))
                    (nginx-server-configuration
                      (server-name '("nonguix.localhost"))
                      (listen '("*:80"))
                      (locations
                       (list
                        (nginx-location-configuration
                          (uri "/")
                          (body
                           '("resolver 80.80.80.80 ipv6=off;"
                             "proxy_pass https://nonguix-proxy.ditigal.xyz/;"
                             "proxy_set_header Host nonguix-proxy.ditigal.xyz;"
                             "proxy_ssl_server_name on;"
                             "client_max_body_size 0;"
                             "proxy_busy_buffers_size 512k;"
                             "proxy_buffers 4 512k;"
                             "proxy_buffer_size 256k;"
                             "add_header Access-Control-Allow-Origin *;"))))))))))
       (elogind-service))
      (modify-services %base-services
        (guix-service-type
         config =>
         (guix-configuration
           (channels %channels-docker-image)
           (guix (guix-for-channels %channels-docker-image))
           (authorized-keys
            (append
             (list
              (local-file "/etc/substitutes/guix.wugi.info.pub")
              (local-file "/etc/substitutes/vm1.wugi.info.pub")
              (local-file "/etc/substitutes/vm2.wugi.info.pub")
              (local-file "/etc/substitutes/mirror.brielmaier.net.pub")
              (local-file "/etc/substitutes/substitutes.nonguix.org.pub")
              (local-file "/etc/substitutes/bordeaux.guix.gnu.org.pub"))
             %default-authorized-guix-keys))
           (substitute-urls '("http://guix.localhost"
                              "http://nonguix.localhost"))))
        (syslog-service-type
         config =>
         (syslog-configuration
           (extra-options '("--rcfile=/etc/syslog.conf"
                            "--no-forward"
                            "--no-unixaf"
                            "--no-klog")))))))

    (sudoers-file (plain-file "sudoers"
                              (string-join `("Defaults:root runcwd=*"
                                             "root ALL=(ALL) ALL"
                                             "%wheel ALL=(ALL) ALL"
                                             "oleg ALL=(ALL) NOPASSWD:ALL")
                                           "\n")))))
