(define-module (wigust packages urho3d)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xorg))

(define-public urho3d
  (package
    (name "urho3d")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/urho3d/Urho3D"
                                  "/archive/" version ".tar.gz"))

              (sha256
               (base32
                "1im63n9rx8qq1ks6aq6jrl5ikxg87hqhsy06miqc317hpm1iyav5"))))
    (build-system cmake-build-system)
    (inputs `(("libx11" ,libx11)
              ("libxcursor" ,libxcursor)
              ("libxext" ,libxext)
              ("libxi" ,libxi)
              ("libxinerama" ,libxinerama)
              ("libxrandr" ,libxrandr)
              ("libxrender" ,libxrender)
              ("libxscrnsaver" ,libxscrnsaver)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("pulseaudio" ,pulseaudio)))
    (arguments
     '(#:tests?
       #f ; There are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'use-full-library-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "../build/Source/ThirdParty/SDL/include/generated/SDL_config.h"
               (("#define SDL_AUDIO_DRIVER_ALSA_DYNAMIC \"libasound\\.so\\.2\"")
                (string-append "#define SDL_AUDIO_DRIVER_ALSA_DYNAMIC \""
                               (assoc-ref inputs "alsa-lib")
                               "/lib/libasound.so.2"
                               "\""))
               (("#define SDL_AUDIO_DRIVER_PULSEAUDIO_DYNAMIC \"libpulse-simple\\.so\\.0\"")
                (string-append "#define SDL_AUDIO_DRIVER_PULSEAUDIO_DYNAMIC \""
                               (assoc-ref inputs "pulseaudio")
                               "/lib/libpulse-simple.so.0"
                               "\"")))
             #t)))))
    (home-page "https://urho3d.github.io/")
    (synopsis "Cross-platform 2D and 3D game engine")
    (description "Cross-platform 2D and 3D game engine")
    (license license:expat)))

(define-public urho3d-checkout
  (let ((commit "4af4148048a6443154c6f0498d32626d10f1e86b")
        (revision "1"))
    (package
      (inherit urho3d)
      (name "urho3d-checkout")
      (version (string-append (package-version urho3d) "-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/urho3d/Urho3D")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1nchxswrkfgzsxghi6c55b27zy9127ni7yvnb4xw2xdddyfm1wl0")))))))
