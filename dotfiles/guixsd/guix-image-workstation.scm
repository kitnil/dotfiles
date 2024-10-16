;; This is an operating system configuration template for a "Docker image"
;; setup, so it has barely any services at all.

;; guix system image -t docker --network ./docker-image-workstation.scm
;; skopeo copy docker-archive:/gnu/store/…-tor-docker-pack.tar.gz docker-daemon:example.org:5000/tor:latest --insecure-policy
;; docker run --network=host --security-opt seccomp=unconfined --detach --name tor --network=host example.org:5000/tor
;; docker exec --detach tor /gnu/store/…-tor-0.4.6.10/bin/tor -f /gnu/store/…-torrc

(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services shells)
             (gnu services)
             (gnu services guix)
             (gnu packages admin)
             (guix gexp))

(use-package-modules pulseaudio screen ssh terminals)
(use-service-modules base desktop dbus shepherd)

(use-modules (manifests wm)
             (services desktop)
             (home config)
             (home services desktop)
             (home services terminals))

(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services sound)
             (gnu services)
             (gnu packages admin)
             (guix gexp))

(use-modules (nongnu packages mozilla))

(define oleg-home
  (home-environment
   (packages (append (list alacritty firefox htop openssh pavucontrol)
                     packages-wm))
   (services (list (service home-dbus-service-type)
                   (service home-pipewire-service-type)
                   (service home-bash-service-type
                            (home-bash-configuration
                             (guix-defaults? #t)
                             (bash-profile (list (plain-file "bash-profile" "\
     export HISTFILE=$XDG_CACHE_HOME/.bash_history")))))
                   (simple-service 'test-config
                        home-xdg-configuration-files-service-type
                        (list `("test.conf"
                                ,(plain-file "tmp-file.txt"
                                             "the content of
                                               ~/.config/test.conf"))))
                   (service home-files-service-type)
                   (simple-service 'sway-config
                                   home-files-service-type
                                   (list `(".config/sway/config" ,(local-file (string-append %project-directory "/dot_config/sway/pc0.config")))
                                         `(".xkb/symbols/custom" ,(local-file (string-append %project-directory "/dot_xkb/symbols/custom")))))
                   (service home-sway-service-type)
                   home-alacritty-service))))

(define container-mingetty-service-type
  (service-type (name 'mingetty)
                (extensions (list (service-extension shepherd-root-service-type
                                                     (@@ (gnu services base) mingetty-shepherd-service))))
                (description
                 "Provide console login using the @command{mingetty}
program.")))

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
                                                "audio" "video"))
                        (password (crypt "oleg" "$6$abc")))
                       (user-account (inherit %root-account)
                                     (password (crypt "root" "$6$abc"))))
                 %base-user-accounts))

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

  ;; Globally-installed packages.
  (packages (append (list screen)
                    %base-packages))

  (services (append (list (service guix-home-service-type
                                   `(("oleg" ,oleg-home)))
                          (dbus-service)
                          (elogind-service)
                          seatd-service
                          (service container-mingetty-service-type
                                   (mingetty-configuration (tty "tty2"))))
                    %base-services)))
