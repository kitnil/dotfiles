(define-module (wugi manifests python)
  #:use-module (guix profiles)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:export (%python-manifest))

(define (%python-manifest)
  (define audio
    (list python-aiohttp
          python-numpy
          python-pyaudio
          python-vosk
          python-websocket-client))

  (packages->manifest (append audio)))
