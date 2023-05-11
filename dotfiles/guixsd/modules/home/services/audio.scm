(define-module (home services audio)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages mail)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (home config)
  #:use-module (gnu services)
  #:use-module (wigust packages audio)
  #:export (home-scream-service-type
            scream-configuration))

(define-record-type* <scream-configuration>
  scream-configuration make-scream-configuration
  scream-configuration?
  (scream scream-configuration-scream       ;<package>
          (default scream))
  (interface scream-configuration-interface)) ;string

(define (home-scream-shepherd-service config)
  (list (shepherd-service
         (documentation "Scream audio service.")
         (provision (list 'scream))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (scream-configuration-scream config)
                                        "/bin/scream")
                         "-i" #$(scream-configuration-interface config)
                         "-u" "-p" "16400")))
         (stop #~(make-kill-destructor)))))

(define home-scream-service-type
  (service-type (name 'home-scream)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-scream-shepherd-service)))
                (default-value '())
                (description
                 "Install and configure the scream.")))
