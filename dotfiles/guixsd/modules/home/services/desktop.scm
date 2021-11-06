(define-module (home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages haskell-apps)
  #:export (home-greenclip-service-type
            greenclip-configuration))

(define-record-type* <greenclip-configuration>
  greenclip-configuration make-greenclip-configuration
  greenclip-configuration?
  (greenclip greenclip-configuration-greenclip ;<package>
             (default greenclip)))

(define (home-greenclip-shepherd-service config)
  (let ((greenclip (greenclip-configuration-greenclip config)))
    (list (shepherd-service
           (documentation "User greenclip.")
           (provision '(greenclip))
           (start #~(make-forkexec-constructor
                     (list #$(file-append greenclip "/bin/greenclip")
                           "daemon")
                     #:log-file (string-append
                                 (or (getenv "XDG_LOG_HOME")
                                     (format #f "~a/.local/var/log"
                                             (getenv "HOME")))
                                 "/greenclip.log")))
           (stop #~(make-kill-destructor))))))

(define home-greenclip-service-type
  (service-type (name 'home-greenclip)
                (extensions
                 (list (service-extension home-shepherd-service-type
                                          home-greenclip-shepherd-service)))
                (default-value (greenclip-configuration))
                (description
                 "Run greenclip clipboard manager daemon.")))
