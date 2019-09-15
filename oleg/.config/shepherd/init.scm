(use-modules (shepherd service))

(define %bin-directory "/home/oleg/.guix-profile/bin/")

(define redshift
  (make <service>
    #:docstring '("Adjust the color temperature of your screen.")
    #:provides '(redshift)
    #:start (make-forkexec-constructor
             `(,(string-append %bin-directory "redshift") "-l" "59:57"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(define transmission
  (make <service>
    #:docstring '("Light-weight BitTorrent client")
    #:provides '(transmission)
    #:start (make-forkexec-constructor
             (list (string-append %bin-directory "transmission-daemon")))
    #:stop (make-kill-destructor)
    #:respawn? #t))

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

(register-services emacs-service)
(start 'emacs)

(action 'shepherd 'daemonize)
