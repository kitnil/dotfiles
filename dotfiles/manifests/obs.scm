(use-modules (gnu packages video)
             (packages video))

(packages->manifest
 (list
  obs-exporter
  obs-pipewire-audio-capture
  obs-with-cef
  obs-wlrobs))
