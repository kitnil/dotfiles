(define-module (packages video)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (packages chromium)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

(define-public obs-exporter
  (let ((commit "ebe35cbe8b963395a39066a83e31355c74f986d2"))
    (package
      (name "obs-exporter")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method url-fetch)
         (uri "https://iso.wugi.info/obs-studio-exporter.so")
         (sha256
          (base32
           "1rcw3cdsdp5ih24j2l5bln9af9fvp60dgzgi2q52gnb9xqqa4pwn"))))
      (build-system binary-build-system)
      (arguments
       `(#:strip-binaries? #f
         #:install-plan
         '(("obs-studio-exporter.so"
            "lib/obs-plugins/obs-studio-exporter.so"))
         #:validate-runpath? #f))
      (home-page "https://github.com/lukegb/obs_studio_exporter")
      (synopsis
       "Prometheus exporter for metrics from OBS Studio")
      (description
       "Exports metrics from OBS Studio in a Prometheus-compatible format.")
      (supported-systems '("x86_64-linux"))
      (license license:asl2.0))))

;; XXX: obs-stroke-glow-shadow does not compile
(define-public obs-stroke-glow-shadow
  (let ((commit "b9e6e7c542820cda922c1816af5527413f3d69f8"))
    (package
      (name "obs-stroke-glow-shadow")
      (version (git-version "1.0.2" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/FiniteSingularity/obs-stroke-glow-shadow")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "06x0i9vpm49i6lyzpmiwlqyk5bvsfqxb3929xj9jlqpqlvcpbx3c"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:modules '((guix build cmake-build-system)
                    (guix build utils))
        #:tests? #f ;no tests
        #:configure-flags
        #~(list (string-append "-DLIBOBS_INCLUDE_DIR="
                               #$(this-package-input "obs") "/lib")
                "-DBUILD_OUT_OF_TREE=On"
                "-Wno-dev"
                "-DCMAKE_C_FLAGS=-Wno-stringop-overflow")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'fix-effects
              (lambda _
                (mkdir-p (string-append #$output "/share/obs/obs-plugins/obs-stroke-glow-shadow"))
                (rename-file (string-append #$output "/data/obs-plugins/obs-stroke-glow-shadow/shaders")
                             (string-append #$output "/share/obs/obs-plugins/obs-stroke-glow-shadow/shaders")))))))
      (inputs (list obs qtbase-5))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:gpl2))))

(define-public obs-teleport
  (package
    (name "obs-teleport")
    (version "0.7.2")
    (source (local-file "/srv/hdd1/Downloads/obs-teleport/linux-x86_64/obs-teleport.so"))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          ,(string-append "/lib/obs-plugins/obs-teleport.so")))
       #:phases
       (modify-phases %standard-phases
         (delete 'patch-source-shebangs)
         (delete 'patch-generated-file-shebangs)
         (delete 'patch-shebangs)
         ;; (delete 'validate-runpath)
         )))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public obs-markdown
  (package
    (name "obs-markdown")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/exeldro/obs-markdown")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04c091b6fi334q0wjkcd27hipd12qir0dwyyqrzyfq2qa1l51k89"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils))
      #:tests? #f ;no tests
      #:configure-flags
      #~(list (string-append "-DLIBOBS_INCLUDE_DIR="
                             #$(this-package-input "obs") "/lib")
              "-DBUILD_OUT_OF_TREE=On"
              "-Wno-dev")))
    (inputs (list obs qtbase-5))
    (home-page "https://github.com/exeldro/obs-markdown")
    (synopsis "Plugin for OBS Studio to add Markdown sources.")
    (description "This OBS plugin lets you type markdown which is convert to html and displayed
using a Browser Source.  The style be changed using CSS.")
    (license license:gpl2)))

(define-public obs-scale-to-sound
  (package
    (name "obs-scale-to-sound")
    (version "1.2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dimtpap/obs-scale-to-sound")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q903g9g84ikp1hqc6myqsd6bxwlf3f406bj3a5nrybqzjwqr8rp"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils))
      #:tests? #f ;no tests
      #:configure-flags
      #~(list (string-append "-DLIBOBS_INCLUDE_DIR="
                             #$(this-package-input "obs") "/lib")
              "-DBUILD_OUT_OF_TREE=On"
              "-Wno-dev")))
    (inputs (list obs qtbase-5))
    (home-page "https://github.com/dimtpap/obs-scale-to-sound")
    (synopsis "OBS filter plugin that scales a source reactively to sound levels")
    (description "Plugin for OBS Studio that adds a filter which makes a source scale based on
the audio levels of any audio source you choose.")
    (license license:gpl2)))

(define-public obs-waveform
  (package
    (name "obs-waveform")
    (version "1.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phandasm/waveform")
                    (commit (string-append "v" version))
                    (recursive? #t))) ; for cpu_features git submodule
              (file-name (git-file-name name version))
              (sha256
               (base32
                "148rm9ljvqvh5h8rsi36k14nrv6mb8innkbi252k69vq4pbnf386"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils))
      #:tests? #f ;no tests
      #:configure-flags
      #~(list (string-append "-DLIBOBS_INCLUDE_DIR="
                             #$(this-package-input "obs") "/lib")
              "-DBUILD_OUT_OF_TREE=On"
              "-Wno-dev"
              (string-append "-DCMAKE_CXX_FLAGS=-fPIC "
                             (or (getenv "CXXFLAGS") ""))
              "-DCMAKE_C_FLAGS=-fPIC")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'obs-plugins
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output "/lib/obs-plugins"))
              (symlink
               (string-append #$output
                              "/waveform/bin/64bit/waveform.so")
               (string-append #$output
                              "/lib/obs-plugins/waveform.so"))
              (mkdir-p (string-append #$output "/share/obs/obs-plugins/waveform"))
              (symlink (string-append #$output "/data/locale")
                       (string-append #$output "/share/obs/obs-plugins/waveform/locale"))
              (symlink (string-append #$output "/waveform/data/gradient.effect")
                       (string-append #$output "/share/obs/obs-plugins/waveform/gradient.effect")))))))
    (inputs (list fftwf obs qtbase-5))
    (native-inputs (list pkg-config))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl2)))
