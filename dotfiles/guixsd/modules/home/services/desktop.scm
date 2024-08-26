(define-module (home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:export (home-greenclip-service-type
            greenclip-configuration
            sway-service))

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

(define sway-program
  (program-file
   "sway-wrapper"
   (with-imported-modules '((guix build utils))
     #~(begin
         (define (wait-for-seatd)
           ;; Wait until someone's listening on udevd's control
           ;; socket.
           (let ((sock (socket AF_UNIX SOCK_SEQPACKET 0)))
             (let try ()
               (catch 'system-error
                 (lambda ()
                   (connect sock PF_UNIX "/run/seatd.sock")
                   (close-port sock))
                 (lambda args
                   (format #t "waiting for seatd...~%")
                   (usleep 500000)
                   (try))))))
         ;; Wait until seatd is up and running.
         (wait-for-seatd)

         (setenv "DESKTOP_SESSION"
                 "sway")
         (setenv "XDG_CURRENT_DESKTOP"
                 "sway")
         (setenv "XDG_SESSION_DESKTOP"
                 "sway")
         (setenv "XDG_SESSION_TYPE"
                 "wayland")
         (setenv "WLR_BACKENDS"
                 "headless,libinput")

         (execl #$(file-append sway "/bin/sway")
                "sway")))))

(define sway-service
  (simple-service 'sway home-shepherd-service-type
                  (list
                   (shepherd-service
                    (provision '(sway))
                    (documentation "Run sway.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list #$sway-program)))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))
