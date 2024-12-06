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
  #:use-module (nonguix build-system binary)
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

(define-public ndi
  (package
    (name "ndi")
    (version "5.6.1") ;NDI SDK for Linux/Version.txt
    ;; https://downloads.ndi.tv/SDK/NDI_SDK_Linux/Install_NDI_SDK_v5_Linux.tar.gz
    (source (local-file "/home/oleg/Install_NDI_SDK_v5_Linux.tar.gz"))
    (build-system trivial-build-system)
    (inputs (list bash-minimal tar findutils coreutils gawk gzip tar glibc patchelf `(,gcc "lib") avahi))
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append
                   #$(this-package-input "gzip") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "avahi") "/bin"
                   ":" #$(this-package-input "bash-minimal") "/bin"
                   ":" #$(this-package-input "gawk") "/bin"
                   ":" #$(this-package-input "findutils") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "coreutils") "/bin"
                   ":" #$(this-package-input "patchelf") "/bin"))
          (invoke "tar" "-xf" #$(this-package-native-input "source"))
          (system "echo y | bash -x ./Install_NDI_SDK_v5_Linux.sh")
          ;; Install binaries.
          (mkdir-p (string-append #$output "/bin"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-interpreter"
                              (string-append #$(this-package-input "glibc")
                                             "/lib/ld-linux-x86-64.so.2")
                              (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file)
                                 (string-append #$output "/bin/" file)))
                    '("ndi-benchmark"
                      "ndi-free-audio"
                      "ndi-directory-service"))
          (invoke "patchelf"
                  "--set-interpreter"
                  (string-append #$(this-package-input "glibc")
                                 "/lib/ld-linux-x86-64.so.2")
                  "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                  (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"))
          (copy-file "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"
                     (string-append #$output "/bin/ndi-record"))
          ;; Install libraries.
          (mkdir-p (string-append #$output "/lib"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                              (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file)
                                 (string-append #$output "/lib/" file)))
                    '("libndi.so.5.6.1"))
          (with-directory-excursion (string-append #$output "/lib")
            (for-each (lambda (file)
                        (symlink "libndi.so.5.6.1" file))
                      '("libndi.so.5"
                        "libndi.so")))
          ;; Install misc.
          (for-each (lambda (directory)
                      (mkdir-p (string-append #$output "/" directory))
                      (copy-recursively (string-append "NDI SDK for Linux/" directory)
                                        (string-append #$output "/" directory)))
                    '("include" "examples"))
          (mkdir-p (string-append #$output "/doc"))
          (for-each (lambda (directory)
                      (copy-recursively directory
                                        (string-append #$output "/doc")))
                    '("licenses" "logos")))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public obs-ndi
  (package
    (name "obs-ndi")
    (version "4.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Palakis/obs-ndi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wsb1k0jilcn6gqgpq5kq8hjiwnb6mi2w32fsqgb88iicwj1qa3y"))
              (patches (append (search-patches "hardcode-ndi-path.patch")
                               (search-patches "obs-ndi-add-additional-latency-mode.patch")))))
    (build-system cmake-build-system)
    (inputs
     (list ndi obs qtbase-5))
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'ndi
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((ndi #$(this-package-input "ndi")))
                (substitute* "src/obs-ndi.cpp"
                  (("@NDI@") ndi))
                (delete-file-recursively "lib/ndi")
                (symlink (string-append ndi "/include")
                         "lib/ndi"))))
          (add-after 'install 'obs-plugins
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output "/lib/obs-plugins"))
              (symlink
               (string-append #$output
                              "/obs-plugins/64bit/obs-ndi.so")
               (string-append #$output
                              "/lib/obs-plugins/obs-ndi.so")))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public ndi-4
  (package
    (name "ndi-4")
    (version "4") ;NDI SDK for Linux/Version.txt
    ;; https://downloads.ndi.tv/SDK/NDI_SDK_Linux/Install_NDI_SDK_v5_Linux.tar.gz
    (source (local-file "/home/oleg/src/git.puscii.nl/puppetexp/puppet-sms/files/InstallNDISDK_v4_Linux.sh"))
    (build-system trivial-build-system)
    (inputs (list bash-minimal tar findutils coreutils gawk gzip tar glibc patchelf `(,gcc "lib") avahi))
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append
                   #$(this-package-input "gzip") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "avahi") "/bin"
                   ":" #$(this-package-input "bash-minimal") "/bin"
                   ":" #$(this-package-input "gawk") "/bin"
                   ":" #$(this-package-input "findutils") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "coreutils") "/bin"
                   ":" #$(this-package-input "patchelf") "/bin"))
          (system (string-append "echo y | bash -x " #$(this-package-native-input "source")))
          ;; Install binaries.
          (mkdir-p (string-append #$output "/bin"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-interpreter"
                              (string-append #$(this-package-input "glibc")
                                             "/lib/ld-linux-x86-64.so.2")
                              (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file)
                                 (string-append #$output "/bin/" file)))
                    '("ndi-directory-service"))
          (invoke "patchelf"
                  "--set-interpreter"
                  (string-append #$(this-package-input "glibc")
                                 "/lib/ld-linux-x86-64.so.2")
                  "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                  (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"))
          (copy-file "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"
                     (string-append #$output "/bin/ndi-record"))
          ;; Install libraries.
          (mkdir-p (string-append #$output "/lib"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                              (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file)
                                 (string-append #$output "/lib/" file)))
                    '("libndi.so.4.0.1"))
          (with-directory-excursion (string-append #$output "/lib")
            (for-each (lambda (file)
                        (symlink "libndi.so.4.0.1" file))
                      '("libndi.so.4"
                        "libndi.so")))
          ;; Install misc.
          (for-each (lambda (directory)
                      (mkdir-p (string-append #$output "/" directory))
                      (copy-recursively (string-append "NDI SDK for Linux/" directory)
                                        (string-append #$output "/" directory)))
                    '("include" "examples"))
          (mkdir-p (string-append #$output "/doc"))
          (for-each (lambda (directory)
                      (copy-recursively directory
                                        (string-append #$output "/doc")))
                    '("licenses" "logos")))))
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
