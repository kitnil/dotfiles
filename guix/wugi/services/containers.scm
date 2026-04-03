(define-module (wugi services containers)
  #:use-module (gnu packages containers)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (container-configuration
            container-configuration?
            container-service-type))

(define (wait-for-files-services files)
  (map (lambda (file)
         (shepherd-service
           (provision
            (list (string->symbol (string-append "wait-for-file-" file))))
           (start
            #~(lambda ()
                (let loop ((i 20))
                  (cond ((file-exists? #$file)
                         #t)
                        ((zero? i)
                         (error "file didn't show up" #$file))
                        (else
                         (pk 'wait-for-file #$file)
                         (sleep 1)
                         (loop (- i 1)))))))
           (stop #~(lambda ()
                     #f))
           (respawn? #f)))
       (delete-duplicates (apply append files))))

(define wait-for-file-service-type
  (service-type
    (name 'wait-for-file)
    (extensions (list (service-extension shepherd-root-service-type
                                         wait-for-files-services)))
    (default-value '())
    (compose append)
    (extend append)
    (description "Wait for file to show up.")))

;;; Commentary:
;;;
;;; This module provides a service definition for the container container service.
;;;
;;; Code:

(define-record-type* <container-configuration>
  container-configuration make-container-configuration
  container-configuration?
  (container container-configuration-container ;<package>
        (default crun))
  (bundle container-configuration-bundle) ;string
  (config-file container-configuration-config-file ;<file-like> object
               (default #f))
  (name container-configuration-name) ;string
  (requirement container-configuration-requirement ;list of symbols
               (default '()))
  (respawn? container-configuration-respawn? ;boolean
            (default #f))
  (auto-start? container-configuration-auto-start? ;boolean
               (default #f))
  (wait-for-files container-configuration-wait-for-files ;list of strings
                  (default #f)))

(define (container-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (mkdir-p "/var/log/container"))))

(define (container-log-rotations config)
  (list
   (string-append "/var/log/container/"
                  (container-configuration-name config)
                  ".log")))

(define container-shepherd-service
  (match-lambda
    (($ <container-configuration> container bundle config-file name requirement respawn? auto-start?)
     (list
      (shepherd-service
       (provision
        (list
         (string->symbol (string-append "container-" name))))
       (documentation "Run container.")
       (requirement requirement)
       (start #~(make-forkexec-constructor
                 (list (string-append #$crun "/bin/crun")
                       "run"
                       "--bundle" #$bundle
                       #$name)
                 #:log-file #$(string-append "/var/log/container/"
                                             name ".log")))
       (respawn? respawn?)
       (auto-start? auto-start?)
       (stop #~(make-kill-destructor)))))))

(define container-service-type
  (service-type (name 'container)
                (extensions (list (service-extension activation-service-type
                                                     container-activation)
                                  (service-extension shepherd-root-service-type
                                                     container-shepherd-service)
                                  (service-extension log-rotation-service-type
                                                     container-log-rotations)
                                  (service-extension profile-service-type
                                                     (lambda (config)
                                                       (list (container-configuration-container config))))
                                  (service-extension wait-for-file-service-type
                                                     (lambda (config)
                                                       (container-configuration-wait-for-files config)))))
                (description "Run container.")))

;;; containers.scm ends here
