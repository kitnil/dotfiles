(define-module (home services mail)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages mail)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (home-goimapnotify-service-type
            goimapnotify-configuration

            mbsync-imap-account-configuration
            mbsync-imap-store-configuration
            mbsync-maildir-store-configuration
            mbsync-channel-configuration
            mbsync-configuration
            mbsync-config-file
            home-mbsync-service-type))

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

(define uglify-field-name
  (match-lambda
    ('imap-account "IMAPAccount")
    ('ssl-type "SSLType")
    ('imap-store "IMAPStore")
    ('maildir-store "MaildirStore")
    ('channel "Channel")
    ('group "Group")
    ('pass-cmd "PassCmd")
    (key (string-concatenate (map string-titlecase
                                  (string-split (symbol->string key) #\-))))))

(define (serialize-field field-name val)
  (format #f "~a ~a\n" (uglify-field-name field-name) val))
(define (serialize-field-list field-name val)
  (serialize-field field-name (string-join val)))

(define (enclose-quotes s)
  (string-append "\"" s "\""))

(define (serialize-string field-name val)
  (serialize-field field-name (match field-name
                                ('pass-cmd
                                 (enclose-quotes val))
                                (_ val))))
(define-maybe string)

(define (string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x) (not (string-index x #\,))))
                val)))
(define (serialize-string-list field-name val)
  (serialize-field-list field-name val))
(define-maybe string-list)

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))
(define (serialize-non-negative-integer field-name val)
  (serialize-field field-name (number->string val)))
(define-maybe non-negative-integer)

(define-configuration mbsync-imap-account-configuration
  (imap-account
   (string "")
   "IMAP account name.  An example value would be @samp{alice}.")
  (host
   (string "")
   "IMAP server host.  An example value would be @samp{imap.example.org}.")
  (user
   (string "")
   "IMAP user.  An example value would be @samp{alice@imap.example.org}.")
  (pass-cmd
   (maybe-string 'disabled)
   "Shell command to obtain a password.  An example value would be
@samp{pass show imap.example.org/alice}")
  (auth-mechs
   (string-list '("LOGIN"))
   "List  of acceptable authentication mechanisms.")
  (ssl-type
   (maybe-string "None")
   "Connection encryption method.")
  (certificate-file
   (maybe-string "/etc/ssl/certs/ca-certificates.crt")
   "File containing additional X.509 certificates used to verify server
identities.")
  (pipeline-depth
   (maybe-non-negative-integer 50)
   "Maximum number of IMAP commands which can be simultaneously in flight."))

(define-configuration mbsync-imap-store-configuration
  (imap-store
   (string "")
   "IMAP store name.  An example value would be @samp{alice-remote}.")
  (account
   (string "")
   "Specify which IMAP4 Account to use.  An example value would be
@samp{alice}."))

(define-configuration mbsync-maildir-store-configuration
  (maildir-store
   (string "")
   "Maildir store name.  An example value would be @samp{alice-local}.")
  (path
   (string "~/Maildir/")
   "The location of the store in the server's file system.")
  (inbox
   (maybe-string 'disabled)
   "The location of the INBOX.  An example value would be
@samp{~/Maildir/alice}.")
  (sub-folders
   (string "Verbatim")
   "The on-disk folder naming style used for hierarchical mailboxes."))

(define-configuration mbsync-channel-configuration
  (channel
   (string "")
   "Channel name.  An example value would be @samp{alice}.")
  (far
   (string "")
   "Specify the far resp Store to be connected by the Channel.  An
example value would be @samp{:alice-remote:}.")
  (near
   (string "")
   "Specify the near resp Store to be connected by the Channel.  An example
value would be @samp{:majordomo-local:}.")
  (patterns
   (maybe-string-list 'disabled)
   "Instead of synchronizing only one mailbox pair, synchronize all mailboxes
that match the patterns. An example value would be @samp{'(\"INBOX\")}.")
  (sync
   (maybe-string-list 'disabled)
   "Synchronization operation to perform.  An example value would be
@samp{'(\"Pull\")}.")
  (max-messages
   (maybe-non-negative-integer 'disabled)
   "Maximum number of messages to keep in each near side mailbox.")
  (expunge
   (maybe-string 'disabled)
   "Permanently remove all messages marked for deletion."))

(define (serialize-mbsync-imap-account-configuration field-name val)
  (serialize-configuration val mbsync-imap-account-configuration-fields))
(define (mbsync-imap-account-configuration-list? val)
  (and (list? val) (and-map mbsync-imap-account-configuration? val)))
(define (serialize-mbsync-imap-account-configuration-list field-name val)
  #~(string-append
     #$@(map (lambda (val)
               #~(string-append
                  #$(serialize-mbsync-imap-account-configuration field-name val)
                  "\n"))
             val)))

(define (serialize-mbsync-imap-store-configuration field-name val)
  (serialize-configuration val mbsync-imap-store-configuration-fields))
(define (mbsync-imap-store-configuration-list? val)
  (and (list? val) (and-map mbsync-imap-store-configuration? val)))
(define (serialize-mbsync-imap-store-configuration-list field-name val)
  #~(string-append
     #$@(map (lambda (val)
               #~(string-append
                  #$(serialize-mbsync-imap-store-configuration field-name val)
                  "\n"))
             val)))

(define (serialize-mbsync-maildir-store-configuration field-name val)
  (serialize-configuration val mbsync-maildir-store-configuration-fields))
(define (mbsync-maildir-store-configuration-list? val)
  (and (list? val) (and-map mbsync-maildir-store-configuration? val)))
(define (serialize-mbsync-maildir-store-configuration-list field-name val)
  #~(string-append
     #$@(map (lambda (val)
               #~(string-append
                  #$(serialize-mbsync-maildir-store-configuration field-name val)
                  "\n"))
             val)))

(define (serialize-mbsync-channel-configuration field-name val)
  (serialize-configuration val mbsync-channel-configuration-fields))
(define (mbsync-channel-configuration-list? val)
  (and (list? val) (and-map mbsync-channel-configuration? val)))
(define (serialize-mbsync-channel-configuration-list field-name val)
  #~(string-append
     #$@(map (lambda (val)
               #~(string-append
                  #$(serialize-mbsync-channel-configuration field-name val)
                  "\n"))
             val)))

(define-configuration mbsync-configuration
  (imap-accounts
   (mbsync-imap-account-configuration-list
    (list (mbsync-imap-account-configuration)))
   "IMAP accounts list.")
  (imap-stores
   (mbsync-imap-store-configuration-list
    (list (mbsync-imap-store-configuration)))
   "IMAP stores list.")
  (maildir-stores
   (mbsync-maildir-store-configuration-list
    (list (mbsync-maildir-store-configuration)))
   "Maildir stores list.")
  (channels
   (mbsync-channel-configuration-list
    (list (mbsync-channel-configuration)))
   "Channels list."))

(define (mbsync-config-file config)
  (mixed-text-file
   "mbsyncrc"
   (serialize-configuration config mbsync-configuration-fields)))

(define (home-mbsync-config-file config)
  (list `("mbsyncrc"
          ,(computed-file "mbsyncrc"
                          #~(begin
                              (with-output-to-file #$output
                                (lambda ()
                                  (display #$(serialize-text-config
                                              #f
                                              (list (mbsync-config-file config)))))))))))

(define (home-mbsync-extensions original-config extension-configs)
  (mbsync-configuration
   (inherit original-config)
   (imap-accounts
    (append-map mbsync-configuration-imap-accounts extension-configs))
   (imap-stores
    (append-map mbsync-configuration-imap-stores extension-configs))
   (maildir-stores
    (append-map mbsync-configuration-maildir-stores extension-configs))
   (channels
    (append-map mbsync-configuration-channels extension-configs))))

(define home-mbsync-service-type
  (service-type (name 'home-mbsync)
                (extensions
                 (list (service-extension home-files-service-type
                                          home-mbsync-config-file)))
                (compose identity)
                (extend home-mbsync-extensions)
                (default-value '())
                (description "Configure isync.")))
