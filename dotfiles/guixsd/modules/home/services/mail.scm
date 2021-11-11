(define-module (home services mail)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages mail)
  #:use-module (ice-9 match)
  #:export (home-goimapnotify-service-type
            goimapnotify-configuration
            mbsync-config-file))

(define-record-type* <goimapnotify-configuration>
  goimapnotify-configuration make-goimapnotify-configuration
  goimapnotify-configuration?
  (goimapnotify  goimapnotify-configuration-goimapnotify       ;<package>
                 (default go-gitlab.com-shackra-goimapnotify))
  (config-file   goimapnotify-configuration-config-file        ;<file-like>
                 (default #f)))

(define (home-goimapnotify-activation config)
  #~(begin
      (mkdir-p (string-append (or (getenv "XDG_LOG_HOME")
                                  (format #f "~a/.local/var/log"
                                          (getenv "HOME")))
                              "/goimapnotify"))))

(define (home-goimapnotify-shepherd-service config)
  (let ((goimapnotify (goimapnotify-configuration-goimapnotify config))
        (config-file (goimapnotify-configuration-config-file config)))
    (list (shepherd-service
           (documentation "User goimapnotify.")
           (provision (list (string->symbol (string-append "goimapnotify-"
                                                           (computed-file-name config-file)))))
           (start #~(make-forkexec-constructor
                     (list #$(file-append goimapnotify "/bin/goimapnotify")
                           "-conf" #$config-file)
                     #:log-file (string-append
                                 (or (getenv "XDG_LOG_HOME")
                                     (format #f "~a/.local/var/log"
                                             (getenv "HOME")))
                                 (string-append "/goimapnotify/"
                                                #$(computed-file-name config-file)
                                                ".log"))))
           (stop #~(make-kill-destructor))))))

(define home-goimapnotify-service-type
  (service-type (name 'home-goimapnotify)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-goimapnotify-shepherd-service)
                       (service-extension
                        home-activation-service-type
                        home-goimapnotify-activation)))
                (default-value '())
                (description
                 "Install and configure the goimapnotify.")))


;;;
;;; mbsync
;;;

(define-syntax-rule (mbsync-config-file clause ...)
  ;; (string-append (config-file-clause config clause) ...)
  (plain-file
   "config-file"
   (string-join (list (match 'clause
                        (('imap-account value)
                         (format #f "~%IMAPAccount ~a" value))
                        (('ssl-type value)
                         (format #f "SSLType ~a" value))
                        (('imap-store value)
                         (format #f "~%IMAPStore ~a" value))
                        (('maildir-store value)
                         (format #f "~%MaildirStore ~a" value))
                        (('channel value)
                         (format #f "~%Channel ~a" value))
                        (('group value)
                         (format #f "~%Group ~a" value))
                        (('pass-cmd value)
                         (format #f "PassCmd ~s" value))
                        ((key value)
                         (format #f "~a ~a"
                                 (string-concatenate (map string-titlecase
                                                          (string-split (symbol->string key)
                                                                        #\-)))
                                 value)))
                      ...)
                "\n")))
