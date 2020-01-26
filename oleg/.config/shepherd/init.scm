(use-modules (shepherd service))

(define %bin-directory "/home/oleg/.guix-profile/bin/")
(define %redshift (string-append %bin-directory "redshift"))

(define redshift-service
  (make <service>
    #:docstring '("Adjust the color temperature of your screen.")
    #:provides '(redshift)
    #:start (make-forkexec-constructor
             (list %redshift "-O" "4000"))
    #:stop (make-system-destructor (string-join (list %redshift "-x")))
    #:respawn? #f))

(define transmission-service
  (make <service>
    #:docstring '("Light-weight BitTorrent client")
    #:provides '(transmission)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "transmission-daemon")
                   "-e" "/home/oleg/.config/shepherd/transmission.log")
             #:log-file "/home/oleg/.config/shepherd/transmission.log")
    #:stop (make-kill-destructor)
    #:respawn? #t))

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

(define emacs-service
  (make <service>
    #:docstring '("Emacs daemon")
    #:provides '(emacs)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "emacs")
                   "--fg-daemon")
             #:log-file "/home/oleg/.config/shepherd/emacs.log")
    #:stop
    (make-system-destructor (string-join (list (string-append %bin-directory "emacsclient")
                                               "--eval" "'(kill-emacs)'")))
    #:respawn? #f))

(define clipmenud-service
  (make <service>
    #:docstring '("Clipmenud daemon")
    #:provides '(clipmenud)
    #:start (make-forkexec-constructor
             (list (string-append "/home/oleg/.nix-profile/bin/clipmenud")))
    #:stop
    (make-kill-destructor)
    #:respawn? #f))

(define dunst-service
  (make <service>
    #:docstring '("Dunst daemon")
    #:provides '(dunst)
    #:start (make-forkexec-constructor
             (list (string-join (list (string-append %bin-directory "dunst")))
                   "-print")
             #:log-file "/home/oleg/.config/shepherd/dunst.log")
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
             (list "/home/oleg/.guix-profile/bin/xterm" "-title" "alerta" "-fa" "Monospace" "-fs" "6" "+sb"
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

(register-services alerta-service
                   clipmenud-service
                   dunst-service
                   keynav-service
                   ;; sxhkd-service
                   emacs-service
                   firefox-service
                   redshift-service
                   transmission-service
                   ;; znc-service
                   kdeconnect-service
                   pulsemixer-service
                   quassel-service)

(for-each start '(clipmenud
                  dunst
                  keynav
                  ;; sxhkd
                  emacs
                  firefox
                  ;; redshift
                  transmission
                  alerta
                  pulsemixer
                  ;; znc
                  kdeconnect
                  quassel))

(action 'shepherd 'daemonize)
