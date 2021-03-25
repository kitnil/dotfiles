(use-modules (srfi srfi-1) (shepherd service))

(define %bin-directory "/home/oleg/.guix-profile/bin/")
(define %redshift (string-append %bin-directory "redshift"))

(define (without-display environ)
  (delete (string-append "DISPLAY=" (getenv "DISPLAY")) environ))

(define redshift-service
  (make <service>
    #:docstring '("Adjust the color temperature of your screen.")
    #:provides '(redshift)
    #:start (make-forkexec-constructor
             (list %redshift "-O" "4000"))
    #:stop (make-system-destructor (string-join (list %redshift "-x")))
    #:respawn? #f))

(define firefox-service
  (make <service>
    #:docstring '("Firefox daemon")
    #:provides '(firefox)
    #:start (make-forkexec-constructor
             (list "/home/oleg/.nix-profile/bin/firefox")
             #:log-file "/home/oleg/.config/shepherd/firefox.log")
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define place-existing-windows-service
  (make <service>
    #:docstring '("Invoke place-existing-windows in StumpWM")
    #:provides '(place-existing-windows)
    #:requires '(firefox quassel)
    #:one-shot? #t
    #:start (make-forkexec-constructor
             (list "/home/oleg/bin/run-place-existing-windows"))
    #:respawn? #f))

(define emacs-service
  (make <service>
    #:docstring '("Emacs daemon")
    #:provides '(emacs)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "emacs")
                   "--fg-daemon")
    #:environment-variables
    (append (without-display (environ))
            (if (any (lambda (str) (string-prefix? "GTK_THEME" str)) (environ))
                '("GTK_THEME=Adwaita:dark")
                '()))
    #:log-file "/home/oleg/.config/shepherd/emacs.log")
    #:stop
    (make-system-destructor
     (string-join (list (string-append %bin-directory "emacsclient")
                        "--eval" "'(kill-emacs)'")))
    #:respawn? #f))

(define dunst-service
  (make <service>
    #:docstring '("Dunst daemon")
    #:provides '(dunst)
    #:start (make-forkexec-constructor (list (string-join (list (string-append %bin-directory "dunst")))))
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define keynav-service
  (make <service>
    #:docstring '("Keynav daemon")
    #:provides '(keynav)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "keynav")))
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define sxhkd-service
  (make <service>
    #:docstring '("Sxhkd daemon")
    #:provides '(sxhkd)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "sxhkd")))
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define alerta-service
  (make <service>
    #:docstring '("Alerta")
    #:provides '(alerta)
    #:start (make-forkexec-constructor
             (list "/home/oleg/.guix-profile/bin/xterm"
                   "-title" "alerta"
                   "-fa" "Monospace" "-fs" "6" "+sb"
                   "-bg" "black" "-fg" "white"
                   "-e" "/home/oleg/.local/bin/alerta top"))
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define pulsemixer-service
  (make <service>
    #:docstring '("Pulsemixer")
    #:provides '(pulsemixer)
    #:start (make-forkexec-constructor
             (list "/home/oleg/.guix-profile/bin/st"
                   "-f" "Monospace:size=8"
                   "-e" "/home/oleg/.guix-profile/bin/pulsemixer"))
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define znc-service
  (make <service>
    #:docstring '("Znc")
    #:provides '(znc)
    #:start (make-forkexec-constructor
             (list "/home/oleg/.guix-profile/bin/znc" "--foreground" "--no-color")
             #:log-file "/home/oleg/.config/shepherd/znc.log")
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define kdeconnect-service
  (make <service>
    #:docstring '("kdeconnect")
    #:provides '(kdeconnect)
    #:start (make-forkexec-constructor
             (list "/home/oleg/.guix-profile/bin/kdeconnect-indicator"))
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define quassel-service
  (make <service>
    #:docstring '("quassel")
    #:provides '(quassel)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "quassel"))
             #:log-file "/home/oleg/.config/shepherd/quassel.log")
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(register-services emacs-service)

(for-each start '(emacs))

(action 'shepherd 'daemonize)
