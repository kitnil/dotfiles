(define-module (wugi manifests obs)
  #:use-module (gnu packages video)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels current-local-file)
  #:use-module (wugi utils)
  #:export (%obs-manifest))

(define (%obs-manifest)
  (define inferior
    (inferior-for-channels %channels-current-local-file
                           #:cache-directory "/home/oleg/.cache/guix/inferiors"))

  (define inferior-packages
    (inferior-eval `(begin
                      (add-to-load-path ,%distro-directory)
                      (use-modules (gnu packages video)
                                   (nongnu packages video)
                                   (srfi srfi-1)
                                   (wigust packages video))
                      (%patch-path (append (list ,%distro-directory)
                                           (%patch-path)))
                      (fold (lambda (package result)
                              (let ((id (object-address package)))
                                (hashv-set! %package-table id package)
                                (cons (list (package-name package)
                                            (package-version package)
                                            id)
                                      result)))
                            '()
                            (list obs-with-cef
                                  ;; plugins
                                  obs-advanced-masks
                                  obs-composite-blur
                                  obs-exporter
                                  obs-gradient-source
                                  (@ (wigust packages video) obs-looking-glass)
                                  obs-move-transition
                                  obs-multi-rtmp
                                  obs-ndi
                                  obs-pipewire-audio-capture
                                  obs-scale-to-sound
                                  obs-shaderfilter
                                  obs-source-clone
                                  obs-source-record
                                  obs-stroke-glow-shadow
                                  obs-waveform
                                  obs-wlrobs)))
                   inferior))

  (packages->manifest (map (match-lambda
                             ((name version id)
                              ((@@ (guix inferior)inferior-package)
                               inferior name version id)))
                           inferior-packages)))
