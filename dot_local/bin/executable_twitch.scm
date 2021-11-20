#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (twitch) -s
!#

(define-module (twitch)
  #:use-module (guix records)
  #:use-module (guix discovery)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-41)
  #:export (main))

(define-record-type* <twitch>
  twitch make-twitch
  twitch?
  (user twitch-user))

(define %twitch-url
  "https://www.twitch.tv")

(define %distro-root-directory
  ;; Absolute file name of the module hierarchy.
  (let ((file (dirname (current-filename))))
    (and (file-exists? file) (canonicalize-path file))))

(define %default-package-module-path
  ;; Default search path for package modules.
  `(,%distro-root-directory))

(define %twitch-module-path
  (let* ((not-colon   (char-set-complement (char-set #\:)))
         (environment (string-tokenize (or (getenv "GUILE_LOAD_PATH") "")
                                       not-colon)))
    (set! %load-path
          (append environment (list %distro-root-directory) %load-path))
    (set! %load-compiled-path
          (append environment (list %distro-root-directory) %load-compiled-path))
    (make-parameter (append environment %default-package-module-path))))

(define* (fold-twitch proc init
                      #:optional
                      (modules (all-modules (%twitch-module-path))))
  (fold-module-public-variables (lambda (object result)
                                  (if (twitch? object)
                                      (proc object result)
                                      result))
                                init
                                modules))
(define (twitch->commands)
  (fold-twitch (lambda (twitch lst)
                 (format #t "mpv ~s ~%"
                         (string-append %twitch-url "/" (twitch-user twitch)))
                 (format #t "firefox ~s ~%"
                         (string-append "https://www.twitch.tv/popout/"
                                        (twitch-user twitch) "/chat?popout="))
                 (newline))
               '()))

(define-public twitch-shyt33
  (twitch
   (user "shyt33")))

(define-public twitch-inv1ve
  (twitch
   (user "inv1ve")))

(define-public twitch-arhont-tv
  (twitch
   (user "arhont_tv")))

(define-public twitch-jeziq
  (twitch
   (user "jeziq")))

(define (main args)
  (twitch->commands))
