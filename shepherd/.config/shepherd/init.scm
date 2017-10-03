(use-modules (shepherd service))

(define %bin-directory "/home/natsu/.guix-profile")

(define redshift
  (make <service>
    #:docstring '("Adjust the color temperature of your screen.")
    #:provides '(redshift)
    #:start (make-forkexec-constructor
             `(,(string-append %bin-directory "/bin/redshift") "-l" "59:57"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services redshift)

(start 'redshift)

(action 'shepherd 'daemonize)
