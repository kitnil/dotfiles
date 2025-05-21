(define-module (wugi manifests obs)
  #:use-module (gnu packages video)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels current-local-file)
  #:use-module (wugi utils)
  #:export (%obs-manifest))

(define (%obs-manifest)
  (define inferior
    (inferior-for-channels %channels-current-local-file))

  (define %obs-package-names
    (list "obs-with-cef"
          "obs-pipewire-audio-capture"
          "obs-wlrobs"
          "obs-looking-glass"
          "obs-ndi"

          "obs-advanced-masks"
          "obs-composite-blur"
          "obs-gradient-source"
          "obs-move-transition"
          "obs-multi-rtmp"
          "obs-scale-to-sound"
          "obs-shaderfilter"
          "obs-stroke-glow-shadow"
          "obs-waveform"

          "obs-exporter"
          "obs-source-clone"
          "obs-source-record"))

  (packages->manifest (map (lambda (package-name)
                             (first
                              (lookup-inferior-packages inferior package-name)))
                           %obs-package-names)))
