(use-modules (guix profiles)
             (gnu packages audio)
             (gnu packages machine-learning))

(define audio
  (list python-pyaudio python-vosk))

(packages->manifest (append audio))
