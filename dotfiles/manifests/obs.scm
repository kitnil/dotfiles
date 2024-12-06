(use-modules (gnu packages video)
             (packages video)
             (guix profiles)
             (nongnu packages video))

(packages->manifest
 (list
  obs-exporter
  obs-ndi
  obs-pipewire-audio-capture
  obs-source-clone
  obs-source-copy
  obs-source-record
  obs-with-cef
  obs-wlrobs))
