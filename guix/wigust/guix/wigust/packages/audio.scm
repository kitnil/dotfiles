(define-module (wigust packages audio)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build utils))

(define-public scream
  (package
    (name "scream")
    (version "4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/duncanthrax/scream")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qslzq074mnix5isq4xc99fqlbrzmk414504iqal33jrv1s6dzll"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;no tests suite
       #:configure-flags '("-DPULSEAUDIO_ENABLE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "Receivers/unix"))))))
    (inputs
     `(("pulseaudio" ,pulseaudio)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/duncanthrax/scream")
    (synopsis "Audio receiver for the Scream virtual network sound card")
    (description "This packages provides an audio receiver for the Scream
virtual network sound card.")
    (license license:ms-pl)))
