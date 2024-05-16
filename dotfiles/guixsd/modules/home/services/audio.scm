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
            scream-configuration

            home-vosk-service-type
            vosk-configuration))

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


;;;
;;; vosk
;;;

(define-record-type* <vosk-configuration>
  vosk-configuration make-vosk-configuration
  vosk-configuration?
  (vosk vosk-configuration-vosk ;string
        (default #f))
  (environment-variables vosk-configuration-environment-variables ;list of strings
                         (default '())))

(define (home-vosk-shepherd-service config)
  (list (shepherd-service
         (documentation "Vosk audio service.")
         (provision (list 'vosk))
         (start #~(make-forkexec-constructor
                   (list #$(vosk-configuration-vosk config))
                   #:environment-variables
                   (append (list #$@(vosk-configuration-environment-variables config))
                           (environ))))
         (stop #~(make-kill-destructor)))))

(define home-vosk-service-type
  (service-type (name 'home-vosk)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-vosk-shepherd-service)))
                (default-value '())
                (description
                 "Install and configure the vosk.")))
