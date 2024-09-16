(define-module (home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (ice-9 format)
  #:export (home-greenclip-service-type
            greenclip-configuration
            sway-configuration
            home-sway-service-type

            wayvnc-configuration
            home-wayvnc-service-type))

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
           ;; Depend on 'x11-display', which sets 'DISPLAY' if an X11 server is
           ;; available, and fails to start otherwise.
           (requirement '(x11-display))
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

(define-record-type* <sway-configuration>
  sway-configuration make-sway-configuration
  sway-configuration?
  (environment-variables sway-configuration-environment-variables ;list of strings
                         (default '())))

(define (home-sway-shepherd-service config)
  (list (shepherd-service
         (documentation "Run sway.")
         (provision '(sway))
         (start #~(make-forkexec-constructor
                   (list #$(file-append bash "/bin/bash")
                         "-i"
                         "-c" (format #f "exec ~a"
                                      #$(file-append sway "/bin/sway")))
                   #:environment-variables
                   (append (list #$@(sway-configuration-environment-variables config))
                           '("DESKTOP_SESSION=sway"
                             "XDG_CURRENT_DESKTOP=sway"
                             "XDG_SESSION_DESKTOP=sway"
                             "XDG_SESSION_TYPE=wayland")
                           (filter (negate
                                    (lambda (str)
                                      (or (string-prefix? "WAYLAND_DISPLAY=" str))))
                                   (environ)))))
         (stop #~(make-kill-destructor)))))

(define home-sway-service-type
  (service-type (name 'home-sway)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-sway-shepherd-service)))
                (default-value (sway-configuration))
                (description
                 "Run sway.")))


;;;
;;; wayvnc
;;;

(define-record-type* <wayvnc-configuration>
  wayvnc-configuration make-wayvnc-configuration
  wayvnc-configuration?
  (wayvnc                wayvnc-configuration-wayvnc                ;string
                         (default #f))
  (arguments             wayvnc-configuration-arguments             ;list of strings
                         (default '()))
  (environment-variables wayvnc-configuration-environment-variables ;list of strings
                         (default '())))

(define (home-wayvnc-shepherd-service config)
  (list (shepherd-service
         (documentation "User wayvnc.")
         (provision '(wayvnc))
         (requirement '(sway))
         (start #~(make-forkexec-constructor
                   (list #$(wayvnc-configuration-wayvnc config)
                         #$@(wayvnc-configuration-arguments config))
                   #:log-file (string-append
                               (or (getenv "XDG_LOG_HOME")
                                   (format #f "~a/.local/var/log"
                                           (getenv "HOME")))
                               "/wayvnc.log")
                   #:environment-variables
                   (append '("WAYLAND_DISPLAY=wayland-1")
                           (environ))))
         (stop #~(make-kill-destructor)))))

(define home-wayvnc-service-type
  (service-type (name 'home-wayvnc)
                (extensions
                 (list (service-extension home-shepherd-service-type
                                          home-wayvnc-shepherd-service)))
                (default-value (wayvnc-configuration))
                (description
                 "Run wayvnc.")))
