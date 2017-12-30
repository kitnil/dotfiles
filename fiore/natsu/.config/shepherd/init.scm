(use-modules (shepherd service))

(define %bin-directory "/home/natsu/.guix-profile/bin/")

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

(register-services redshift transmission)

(start 'transmission)

(action 'shepherd 'daemonize)
