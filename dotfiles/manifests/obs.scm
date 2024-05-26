(use-modules (gnu packages video)
             (packages video))

(packages->manifest
 (list
  obs-exporter
  obs-pipewire-audio-capture
  obs-source-record
  obs-with-cef
  obs-wlrobs))
