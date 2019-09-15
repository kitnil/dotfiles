(use-modules (shepherd service))

(define %bin-directory "/home/oleg/.guix-profile/bin/")
(define %redshift (string-append %bin-directory "redshift"))

(define redshift-service
  (make <service>
    #:docstring '("Adjust the color temperature of your screen.")
    #:provides '(redshift)
    #:start (make-forkexec-constructor
             (list %redshift "-O" "4000")
             #:log-file "/home/oleg/.config/shepherd/redshift.log")
    #:stop (make-system-destructor (string-join (list %redshift "-x")))
    #:respawn? #f))

(define transmission-service
  (make <service>
    #:docstring '("Light-weight BitTorrent client")
    #:provides '(transmission)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "transmission-daemon"))
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
                                               "--eval" "'(let (kill-emacs-hook) (kill-emacs))'")))
    #:respawn? #f))

(register-services emacs-service firefox-service redshift-service transmission-service)
(for-each start '(emacs firefox redshift transmission))
(action 'shepherd 'daemonize)
