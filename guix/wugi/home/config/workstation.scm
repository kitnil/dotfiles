(define-module (wugi home config workstation)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services mail)
  #:use-module (gnu home services niri)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels workstation)
  #:use-module (wugi home config)
  #:use-module (wugi home services audio)
  #:use-module (wugi home services databases)
  #:use-module (wugi home services desktop)
  #:use-module (wugi home services dns)
  #:use-module (wugi home services emacs)
  #:use-module (wugi home services gdb)
  #:use-module (wugi home services gnupg)
  #:use-module (wugi home services groovy)
  #:use-module (wugi home services gtk)
  #:use-module (wugi home services guile)
  #:use-module (wugi home services haskell-apps)
  #:use-module (wugi home services kodi)
  #:use-module (wugi home services linux)
  #:use-module (wugi home services lisp)
  #:use-module (wugi home services mail)
  #:use-module (wugi home services mime)
  #:use-module (wugi home services nano)
  #:use-module (wugi home services nix)
  #:use-module (wugi home services python)
  #:use-module (wugi home services rust-apps)
  #:use-module (wugi home services shell)
  #:use-module (wugi home services terminals)
  #:use-module (wugi home services tmux)
  #:use-module (wugi home services version-control)
  #:use-module (wugi home services video)
  #:use-module (wugi home services web)
  #:use-module (wugi manifests workstation)
  #:use-module (wugi utils)
  #:export (%workstation-home-environment))

(define (manifest->packages manifest)
  "Return the list of packages in MANIFEST."
  (filter-map (lambda (entry)
                (let ((item (manifest-entry-item entry)))
                  (if (or (package? item) (inferior-package? item)) item #f)))
              (manifest-entries manifest)))

(define (packages-from-manifest manifest)
  "Return the list of packages in loaded MANIFEST."
  (let* ((user-module (make-user-module '((guix profiles) (gnu))))
         (manifest    (load* manifest user-module)))
    (manifest->packages manifest)))

(define (%workstation-home-environment)
  (home-environment
    (packages (manifest->packages (%workstation-manifest)))
    (services (list (if (file-exists?
                         (string-append %distro-directory "/wugi/home/config/openssh.scm"))
                        (service home-openssh-service-type
                                 (@ (wugi home config openssh)
                                    %home-openssh-configuration))
                        (service home-openssh-service-type))
                    (service home-dbus-service-type)
                    (service home-pipewire-service-type)
                    (simple-service 'test-config
                                    home-xdg-configuration-files-service-type
                                    (list `("test.conf"
                                            ,(plain-file "tmp-file.txt"
                                                         "the content of
                                               ~/.config/test.conf"))))
                    (service home-files-service-type)
                    (simple-service 'waybar-config
                                    home-files-service-type
                                    (list `(".config/waybar/config.jsonc"
                                            ,(local-file (string-append %distro-directory "/dot_config/waybar/config.jsonc")))
                                          `(".config/waybar/config-1.jsonc"
                                            ,(local-file (string-append %distro-directory "/dot_config/waybar/config-1.jsonc")))
                                          `(".config/waybar/config-2.jsonc"
                                            ,(local-file (string-append %distro-directory "/dot_config/waybar/config-2.jsonc")))
                                          `(".config/waybar/style.css"
                                            ,(local-file (string-append %distro-directory "/dot_config/waybar/style.css")))))
                    (simple-service 'mako-config
                                    home-files-service-type
                                    (list `(".config/mako/config"
                                            ,(local-file (string-append %distro-directory "/dot_config/mako/config")))))
                    (simple-service 'aichat-config
                                    home-files-service-type
                                    (list `(".config/aichat/config.yaml"
                                            ,(local-file (string-append %distro-directory "/dot_config/aichat/config.yaml")))))
                    (service home-wayvnc-service-type)
                    (service home-niri-service-type)
                    (service home-scream-service-type
                             (scream-configuration
                              (port 16400)))
                    (service home-alacritty-service-type)
                    (simple-service 'gnupg-config
                                    home-files-service-type
                                    (list `(".gnupg/gpg.conf"
                                            ,(local-file (string-append %distro-directory "/private_dot_gnupg/gpg.conf")))
                                          `(".gnupg/gpg-agent.conf"
                                            ,(mixed-text-file "gpg-agent.conf" "\
no-grab
pinentry-program " (file-append pinentry "/bin/pinentry") "
pinentry-timeout 5
default-cache-ttl 172800
default-cache-ttl-ssh 172800
max-cache-ttl 172800
max-cache-ttl-ssh 172800
allow-preset-passphrase"))))

                    (simple-service 'bin-manual-scripts
                                    home-files-service-type
                                    (list `("bin/manual-scripts-root-02-net.sh"
                                            ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/03-net.sh")
                                                         #:recursive? #t))
                                          `("bin/manual-scripts-root-03-firefox-twitch-namespace.sh"
                                            ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/04-firefox-twitch-namespace.sh")
                                                         #:recursive? #t))
                                          `("bin/manual-scripts-root-04-mjru-net"
                                            ,(program-file "manual-scripts-root-04-mjru-net"
                                                           #~(let ((gateway "192.168.0.144"))
                                                               (for-each (lambda (network)
                                                                           (system* #$(file-append iproute "/sbin/ip") "route" "add" network "via" gateway))
                                                                         '("172.16.103.0/24"
                                                                           "78.108.80.0/24"
                                                                           "78.108.88.0/24")))))
                                          `("bin/manual-scripts-oleg-01-ssh.sh"
                                            ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/02-ssh.sh")
                                                         #:recursive? #t))
                                          `("bin/manual-scripts-oleg-02-gnupg.sh"
                                            ,(local-file (string-append %distro-directory "/dotfiles/run/guix-workstation/05-gnupg.sh")
                                                         #:recursive? #t))))
                    (simple-service 'bin-namespace-host
                                    home-files-service-type
                                    (list `("bin/namespace-host"
                                            ,(program-file "namespace-host"
                                                           #~(execl #$(file-append openssh "/bin/ssh")
                                                                    "ssh" "-t" "192.168.0.192" "tmux")))))
                    (simple-service 'bin-wl-mirror
                                    home-files-service-type
                                    (map (lambda (wayland-output)
                                           (let ((file-name (string-downcase wayland-output)))
                                             `(,(string-append "bin/" file-name)
                                               ,(program-file file-name
                                                              #~(execl #$(file-append wl-mirror "/bin/wl-mirror")
                                                                       "wl-mirror"
                                                                       "--scaling" "exact"
                                                                       #$wayland-output)))))
                                         '("HEADLESS-1"
                                           "HEADLESS-2"
                                           "HEADLESS-3"
                                           "HEADLESS-4"
                                           "HEADLESS-5"
                                           "HEADLESS-6"
                                           "HEADLESS-7"
                                           "HEADLESS-8")))
                    (simple-service 'bin-bemenu-scripts
                                    home-files-service-type
                                    (list `("bin/bemenu-passmenu"
                                            ,(local-file (string-append %distro-directory "/dot_local/bin/passmenu")
                                                         #:recursive? #t))))
                    home-bash-service
                    (service home-bash-service-type
                             (home-bash-configuration
                               (bashrc
                                (list
                                 (local-file
                                  (string-append %distro-directory "/dot_bashrc"))))
                               (bash-profile
                                (list
                                 (local-file
                                  (string-append %distro-directory "/dot_bash_profile"))))))
                    home-mime-service
                    home-direnv-service
                    home-git-service
                    home-gita-service
                    home-gdb-service
                    home-emacs-state-service
                    home-emacs-service
                    home-nano-service
                    home-inputrc-service
                    home-tmux-service
                    tmuxifier-service
                    home-top-service
                    home-nix-service
                    home-qterminal-service
                    home-gtk-service
                    home-gtkrc-service
                    home-ripgrep-service
                    home-screen-service
                    home-sbcl-service
                    home-python-service
                    home-bind-utils-service
                    ;; home-shellcheck-service
                    home-bin-service
                    home-ghci-service
                    home-groovy-service
                    home-guile-service
                    home-kodi-service
                    home-mailcap-service
                    home-mongo-service
                    home-postgresql-service
                    home-mycli-service
                    home-parallel-service
                    home-youtube-dl-service
                    home-wireplumber-config-service
                    home-mpv-service

                    (simple-service 'looking-glass-wrapper
                                    home-files-service-type
                                    (list `(".local/bin/looking-glass-client-wrapper"
                                            ,(program-file "looking-glass-client-wrapper"
                                                           #~(let ((args (cdr (command-line))))
                                                               (apply execl
                                                                      `(#$(file-append looking-glass-client "/bin/looking-glass-client")
                                                                        "looking-glass-client"
                                                                        "spice:enable" "no"
                                                                        "wayland:warpSupport" "no"
                                                                        "input:grabKeyboard" "no"
                                                                        "win:dontUpscale" "yes"
                                                                        ,@(if (file-exists? "/dev/kvmfr0")
                                                                              '("-f" "/dev/kvmfr0")
                                                                              '())
                                                                        ,@args)))))))

                    (service home-msmtp-service-type
                             (home-msmtp-configuration
                               (default-account "gmail")
                               (extra-content "\
auth on
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt\n")
                               (accounts
                                (list
                                 (msmtp-account
                                   (name "gmail")
                                   (configuration
                                    (msmtp-configuration
                                      (host "smtp.gmail.com")
                                      (port 587)
                                      (user "go.wigust")
                                      (password-eval "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.password-store/myaccount.google.com/apppasswords/go.wigust.gpg"))))))))))))
