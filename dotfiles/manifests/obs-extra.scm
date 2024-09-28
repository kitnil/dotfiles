(use-modules (gnu packages video)
             (packages video))

(packages->manifest
 (list
  obs-advanced-masks
  obs-composite-blur
  obs-gradient-source
  obs-move-transition
  obs-multi-rtmp
  obs-scale-to-sound
  obs-shaderfilter
  obs-source-clone
  obs-source-copy
  obs-stroke-glow-shadow
  obs-waveform))
