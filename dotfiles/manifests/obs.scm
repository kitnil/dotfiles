(define-module (obs)
  #:use-module (gnu packages video)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (nongnu packages video)
  #:use-module (wigust packages video)
  #:use-module (srfi srfi-1))

(define channels
  (include "/home/oleg/.local/share/chezmoi/dotfiles/channels-current-local-file.scm"))

(define cached
  (with-store store
    (cached-channel-instance store
                             channels
                             #:authenticate? #t
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30))))

(define inferior
  (open-inferior cached #:error-port (current-error-port)))

(define %obs-package-names
  (list "obs-with-cef"
        "obs-pipewire-audio-capture"
        "obs-wlrobs"
        "obs-looking-glass"

        ;; "obs-exporter"
        ;; "obs-source-clone"
        ;; "obs-source-record"
        ))

(packages->manifest (append (map (lambda (package-name)
                                   (first
                                    (lookup-inferior-packages inferior package-name)))
                                 %obs-package-names)
                            (list obs-ndi)))
