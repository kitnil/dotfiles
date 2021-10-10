(define-module (home services mail)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (gnu packages mail)
  #:export (home-goimapnotify-service-type))

(define (home-goimapnotify-shepherd-service config)
  (list (shepherd-service
         (documentation "User goimapnotify.")
         (provision '(goimapnotify))
         (start #~(make-forkexec-constructor
                   (list #$(file-append go-gitlab.com-shackra-goimapnotify "/bin/goimapnotify")
                         "-conf" #$(local-file "/home/oleg/.config/imapnotify/gmail.conf"))
                   #:log-file (string-append
                               (or (getenv "XDG_LOG_HOME")
                                   (format #f "~a/.local/var/log"
                                           (getenv "HOME")))
                               "/goimapnotify/gmail.log")))
         (stop #~(make-kill-destructor)))))

(define home-goimapnotify-service-type
  (service-type (name 'home-goimapnotify)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-goimapnotify-shepherd-service)))
                (default-value '())
                (description
                 "Install and configure the goimapnotify.")))
