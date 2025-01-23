(use-modules (guix profiles)
             (gnu packages audio)
             (gnu packages machine-learning)
             (gnu packages python-xyz)
             (gnu packages python-web))

(define audio
  (list python-aiohttp
        python-numpy
        python-pyaudio
        python-vosk
        python-websocket-client))

(packages->manifest (append audio))
