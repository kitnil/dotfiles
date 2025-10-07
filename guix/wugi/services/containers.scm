(define-module (wugi services containers)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (runc-container-configuration
            runc-container-configuration?
            runc-container-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the runc container service.
;;;
;;; Code:

(define-record-type* <runc-container-configuration>
  runc-container-configuration make-runc-container-configuration
  runc-container-configuration?
  (runc runc-container-configuration-runc ;<package>
        (default runc))
  (bundle runc-container-configuration-bundle) ;string
  (config-file runc-container-configuration-config-file ;<file-like> object
               (default #f))
  (name runc-container-configuration-name) ;string
  (requirement runc-container-configuration-requirement ;list of symbols
               (default '()))
  (respawn? runc-container-configuration-respawn? ;boolean
            (default #f))
  (auto-start? runc-container-configuration-auto-start? ;boolean
               (default #f)))

(define (runc-container-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (mkdir-p "/var/log/runc"))))

(define (runc-container-log-rotations config)
  (list
   (log-rotation
    (files
     (list
      (string-append "/var/log/runc/"
                     (runc-container-configuration-name config)
                     ".log"))))))

(define runc-container-shepherd-service
  (match-lambda
    (($ <runc-container-configuration> runc bundle config-file name requirement respawn? auto-start?)
     (list
      (shepherd-service
       (provision
        (list
         (string->symbol (string-append "runc-container-" name))))
       (documentation "Run runc-container.")
       (requirement requirement)
       (start #~(make-forkexec-constructor
                 (list (string-append #$runc "/sbin/runc")
                       "run"
                       "--bundle" #$bundle
                       #$name)
                 #:log-file #$(string-append "/var/log/runc-container/"
                                             name ".log")))
       (respawn? respawn?)
       (auto-start? auto-start?)
       (stop #~(make-kill-destructor)))))))

(define runc-container-service-type
  (service-type (name 'runc-container)
                (extensions (list (service-extension activation-service-type
                                                     runc-container-activation)
                                  (service-extension shepherd-root-service-type
                                                     runc-container-shepherd-service)
                                  (service-extension log-rotation-service-type
                                                     runc-container-log-rotations)))
                (description "Run runc-container.")))

;;; containers.scm ends here
