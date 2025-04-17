(use-modules (gnu home services desktop)
             (gnu home services shells)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services ssh)
             (gnu home services)
             (gnu home)
             (gnu packages gnupg)
             (gnu packages linux)
             (gnu packages ssh)
             (gnu packages virtualization)
             (gnu packages wm)

             (gnu services)
             (guix gexp)
             (guix packages)
             (guix profiles)
             (guix inferior)
             (guix ui)

             (srfi srfi-1)

             (home config)
             (home config openssh)
             (home services audio)
             (home services databases)
             (home services desktop)
             (home services dns)
             (home services emacs)
             (home services gdb)
             (home services gnupg)
             (home services groovy)
             (home services gtk)
             (home services guile)
             (home services haskell-apps)
             (home services kodi)
             (home services linux)
             (home services lisp)
             (home services mail)
             (home services mime)
             (home services nano)
             (home services nix)
             (home services python)
             (home services rust-apps)
             (home services shell)
             (home services terminals)
             (home services tmux)
             (home services version-control)
             (home services video)
             (home services web))

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

(home-environment
 (packages (packages-from-manifest "/home/oleg/.local/share/chezmoi/dotfiles/manifests/pc0.scm"))
 (services (list (service home-openssh-service-type
                          %home-openssh-configuration)
                 (service home-dbus-service-type)
                 (service home-pipewire-service-type)
                 (simple-service 'test-config
                                 home-xdg-configuration-files-service-type
                                 (list `("test.conf"
                                         ,(plain-file "tmp-file.txt"
                                                      "the content of
                                               ~/.config/test.conf"))))
                 (service home-files-service-type)
                 (simple-service 'sway-config
                                 home-files-service-type
                                 (append (list `(".xkb/symbols/custom" ,(local-file (string-append %project-directory "/dot_xkb/symbols/custom"))))
                                         (list `("bin/move" ,(program-file "sway-move"
                                                                           #~(let ((args (cdr (command-line))))
                                                                               (execl #$(file-append sway "/bin/swaymsg") "swaymsg" "move" "workspace" (string-join args)))))
                                               `("bin/workspace" ,(program-file "sway-workspace"
                                                                                #~(let ((args (cdr (command-line))))
                                                                                    (execl #$(file-append sway "/bin/swaymsg") "swaymsg" "workspace" (string-join args)))))
                                               `("bin/workspaces" ,(local-file (string-append %project-directory "/dot_local/bin/executable_sway-workspaces")
                                                                               #:recursive? #t)))))
                 (service home-sway-service-type)
                 (service home-scream-service-type
                          (scream-configuration
                           (port 16400)))
                 (simple-service 'gnupg-config
                                 home-files-service-type
                                 (list `(".gnupg/gpg.conf"
                                         ,(local-file (string-append %project-directory "/private_dot_gnupg/gpg.conf")))
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
                                         ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/03-net.sh")
                                                      #:recursive? #t))
                                       `("bin/manual-scripts-root-03-firefox-twitch-namespace.sh"
                                         ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/04-firefox-twitch-namespace.sh")
                                                      #:recursive? #t))
                                       `("bin/manual-scripts-root-04-mjru-net"
                                         ,(program-file "manual-scripts-root-04-mjru-net"
                                                        #~(let ((gateway "192.168.0.145"))
                                                            (for-each (lambda (network)
                                                                        (system* #$(file-append iproute "/sbin/ip") "route" "add" network "via" gateway))
                                                                      '("172.16.103.0/24"
                                                                        "78.108.80.0/24"
                                                                        "78.108.88.0/24")))))
                                       `("bin/manual-scripts-oleg-01-ssh.sh"
                                         ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/02-ssh.sh")
                                                      #:recursive? #t))
                                       `("bin/manual-scripts-oleg-02-gnupg.sh"
                                         ,(local-file (string-append %project-directory "/dotfiles/run/guix-workstation/05-gnupg.sh")
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
                                         ,(local-file (string-append %project-directory "/dot_local/bin/executable_passmenu")
                                                      #:recursive? #t))))
                 home-bash-service
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
                 home-top-service
                 home-nix-service
                 home-alacritty-service
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
                                                                       ,@args))))))))))
