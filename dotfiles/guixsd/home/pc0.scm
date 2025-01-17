(use-modules (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services mcron)
             (gnu home services shells)
             (gnu home services ssh)
             (gnu home services sound)
	     (gnu services)
             (gnu packages xorg)
	     (guix gexp)
	     (home config)
             (home services audio)
             (home services desktop)
             (home services shell)
             (home services mime)
             (home services gdb)
             (home services version-control)
             (home services emacs)
             (home services tmux)
             (home services linux)
             (home services nix)
             (home services terminals)
             (home services haskell-apps)
             (home services gtk)
             (home services rust-apps)
             (home services lisp)
             (home services python)
             (home services nano)
             (home services dns)
             (home services web)
             (home services gnupg)
             (home services groovy)
             (home services guile)
             (home services kodi)
             (home services mail)
             (home services databases)
             (home services video))

(define xmodmap-script
  (program-file
   "xmodmap"
   #~(begin
       (use-modules (srfi srfi-1)
                    (ice-9 popen)
                    (ice-9 rdelim))

       (define %home
         (and=> (getenv "HOME")
                (lambda (home)
                  home)))

       (define xmodmap
         #$(file-append xmodmap "/bin/xmodmap"))

       (define count (make-parameter 0))

       (let loop ()
         (let* ((port (open-pipe* OPEN_READ xmodmap "-pke"))
                (output (read-string port)))
           (close-pipe port)
           (if (or (< 2 (count))
                   (any (lambda (str)
                          (string= str "keycode 134 = Control_L NoSymbol Control_L"))
                        (string-split (string-trim-right output #\newline) #\newline)))
               #t
               (begin
                 (count (1+ (count)))
                 (system* xmodmap (string-append %home "/.Xmodmap"))
                 (sleep 3)
                 (loop))))))))

(home-environment
 (services
  (list
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (simple-service 'sway-config
                   home-files-service-type
                   (list `(".config/sway/config" ,(local-file (string-append %project-directory "/dot_config/sway/pc0.config")))
                         `(".xkb/symbols/custom" ,(local-file (string-append %project-directory "/dot_xkb/symbols/custom")))))
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
   home-gnupg-service
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

   (service home-scream-service-type
            (scream-configuration
             (interface "eth0")))

   (service home-sway-service-type)

   (service home-wayvnc-service-type
            (wayvnc-configuration
             (wayvnc "/home/oleg/bin/wayvnc")
             (environment-variables
              '("WAYLAND_DISPLAY=wayland-1"))
             (arguments
              '("--output=HEADLESS-3")))))))
