(define-module (wugi home config guixsd)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services sound)
  #:use-module (gnu services)
  #:use-module (gnu packages xorg)
  #:use-module (guix gexp)
  #:use-module (wugi home services audio)
  #:use-module (wugi home services desktop)
  #:use-module (wugi home services shell)
  #:use-module (wugi home services mime)
  #:use-module (wugi home services gdb)
  #:use-module (wugi home services version-control)
  #:use-module (wugi home services emacs)
  #:use-module (wugi home services tmux)
  #:use-module (wugi home services linux)
  #:use-module (wugi home services nix)
  #:use-module (wugi home services terminals)
  #:use-module (wugi home services haskell-apps)
  #:use-module (wugi home services gtk)
  #:use-module (wugi home services rust-apps)
  #:use-module (wugi home services lisp)
  #:use-module (wugi home services python)
  #:use-module (wugi home services nano)
  #:use-module (wugi home services dns)
  #:use-module (wugi home services web)
  #:use-module (wugi home services gnupg)
  #:use-module (wugi home services groovy)
  #:use-module (wugi home services guile)
  #:use-module (wugi home services kodi)
  #:use-module (wugi home services mail)
  #:use-module (wugi home services databases)
  #:use-module (wugi home services video)
  #:export (%pc0-home-environment))

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

(define (%pc0-home-environment)
  (home-environment
   (services
    (list
     (service home-dbus-service-type)
     (service home-pipewire-service-type)
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
     (service home-alacritty-service-type)
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

     (service home-wayvnc-service-type
              (wayvnc-configuration
               (wayvnc "/home/oleg/bin/wayvnc")
               (environment-variables
                '("WAYLAND_DISPLAY=wayland-1"))
               (arguments
                '("--output=HEADLESS-3"))))))))
