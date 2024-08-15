(define-module (packages video)
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

(define-public obs-with-cef
  (package
    (inherit obs)
    (inputs
     (append (package-inputs obs)
             `(("chromium-embedded-framework" ,chromium-embedded-framework-117))))
    (arguments
     (substitute-keyword-arguments (package-arguments obs)
       ((#:configure-flags flags)
        #~(append #$flags
                  '("-DBUILD_BROWSER=ON"
                    "-DCEF_ROOT_DIR=../source/cef")))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'add-cef
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((chromium-embedded-framework
                       #$(this-package-input "chromium-embedded-framework")))
                  (mkdir-p "cef/Release")
                  (mkdir-p "cef/Resources")
                  (for-each (lambda (file)
                              (symlink file (string-append "cef/Release/"
                                                           (basename file)))
                              (symlink file (string-append "cef/Resources/"
                                                           (basename file))))
                            (filter
                             (lambda (file)
                               (not (string= (basename (dirname file))
                                             "locales")))
                             (find-files
                              (string-append chromium-embedded-framework
                                             "/share/cef"))))
                  (symlink (string-append chromium-embedded-framework
                                          "/lib/libcef.so")
                           "cef/Release/libcef.so")
                  (mkdir-p "cef/libcef_dll_wrapper")
                  (symlink (string-append chromium-embedded-framework
                                          "/lib/libcef_dll_wrapper.a")
                           "cef/libcef_dll_wrapper/libcef_dll_wrapper.a")
                  (symlink (string-append chromium-embedded-framework
                                          "/include")
                           "cef/include"))))
            (add-after 'install 'symlink-obs-browser
              ;; Required for lib/obs-plugins/obs-browser.so file.
              (lambda* (#:key outputs #:allow-other-keys)
                (symlink
                 (string-append #$output
                                "/lib/libobs-frontend-api.so.0")
                 (string-append #$output
                                "/lib/obs-plugins/libobs-frontend-api.so.0"))
                (symlink
                 (string-append #$output
                                "/lib/libobs.so.0")
                 (string-append #$output
                                "/lib/obs-plugins/libobs.so.0"))))
            (replace 'wrap-executable
             (lambda* _
               (let ((plugin-path (getenv "QT_PLUGIN_PATH")))
                 (wrap-program (string-append #$output "/bin/obs")
                   `("QT_PLUGIN_PATH" ":" prefix (,plugin-path))
                   `("LD_LIBRARY_PATH" ":" prefix
                     (,(string-append #$(this-package-input "vlc")
                                      "/lib")))))))))))))

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

(define-public obs-advanced-masks
  (package
    (name "obs-advanced-masks")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FiniteSingularity/obs-advanced-masks")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vhilhzdfv0wa8hqz8ffavr272w3d5b75vvldf8rfy9pm5c8xn9n"))))
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
              "-Wno-dev")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'fix-effects
            (lambda _
              (mkdir-p (string-append #$output "/share/obs/obs-plugins/obs-advanced-masks"))
              (rename-file (string-append #$output "/data/obs-plugins/obs-advanced-masks/shaders")
                           (string-append #$output "/share/obs/obs-plugins/obs-advanced-masks/shaders")))))))
    (inputs (list obs qtbase-5))
    (home-page "https://github.com/FiniteSingularity/obs-advanced-masks")
    (synopsis "Advanced masking plugin for OBS")
    (description "OBS Advanced Masks is a project designed to expand the masking
functionalities within OBS Studio.  This plug-in provides filters for users to
create intricate and customized masks for their OBS Scenes and Sources.

@itemize
@item Advanced Masks provides both Alpha Masking and Adjustment Masking.
@item Shape masks allow for dynamically generated Rectangle, Circle,
Elliptical, Regular Polygon, Star, and Heart shaped masks, with many
adjustable parameters.
@item Source Masks allow an existing OBS source to be used as a mask, using
any combination of the red, green, blue, or alpha channels from said source.
@item Image Masks include all of the same functionality as Source Masks, but
applied via a static image (.png, .jpeg, etc).
@item Gradient Masks allow a fading mask using a user-specified gradient.
@end itemize\n")
    (license license:gpl2)))

(define-public obs-composite-blur
  (package
    (name "obs-composite-blur")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FiniteSingularity/obs-composite-blur")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mlbc1zi4bp8xwiq0ynjciysqvlbrxa0v5an9hkzsl9vwxgz9jc9"))))
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
              "-Wno-dev")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'fix-effects
            (lambda _
              (mkdir-p (string-append #$output "/share/obs/obs-plugins/obs-composite-blur"))
              (rename-file (string-append #$output "/data/obs-plugins/obs-composite-blur/shaders")
                           (string-append #$output "/share/obs/obs-plugins/obs-composite-blur/shaders")))))))
    (inputs (list obs qtbase-5))
    (home-page "https://github.com/FiniteSingularity/obs-composite-blur")
    (synopsis "Different blur algorithms for OBS")
    (description "Composite Blur Plugin is a comprehensive blur plugin that
provides blur algorithms and types for all levels of quality and computational
need.

@itemize
@item Composite Blur provides several highly optimized blur algorithms
including Gaussian, Multi-Pass Box, Dual Kawase, and Pixelate.
@item Composite Blur provides multiple blur effects to give a different look
and feel to the blur including Area, Directional, Zoom, Motion, and
Tilt-Shift.
@item Composite Blur also allows setting a Background Source so that it can
properly composite blurred masks, allowing you to properly layer blurred
sources.
@item Finally, Composite Blur provides an option to mask where and how much
blurring occurs on the source via Crop, Rectangle, Circle, Source, and Image
masks.
@end itemize\n")
    (license license:gpl2)))

(define-public obs-source-clone
  (package
    (name "obs-source-clone")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/exeldro/obs-source-clone")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rw0qknlkljzn4rk41g2jjnf113vald5k7kpvxvz0mpaywa6vc6j"))))
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
    (home-page "https://github.com/exeldro/obs-source-clone")
    (synopsis "Plugin for OBS Studio to clone sources")
    (description "")
    (license license:gpl2)))

(define-public obs-move-transition
  (package
    (name "obs-move-transition")
    (version "3.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/exeldro/obs-move-transition")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kni1a8zqqbgx5mmaw4k4chswsy0i9qk89zcbg58mvspz9zzv4id"))))
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
    (home-page "https://github.com/exeldro/obs-move-transition")
    (synopsis "Move transition for OBS Studio")
    (description "Plugin for OBS Studio to move source to a new position during scene
transition.")
    (license license:gpl2)))

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
              (patches (append (search-patches "hardcode-ndi-path.patch")))))
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

(define-public obs-multi-rtmp
  (package
    (name "obs-multi-rtmp")
    (version "0.3.0.2-OBS29.1.1")       ;0.2.8.1-OBS29
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sorayuki/obs-multi-rtmp")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "192zkihn3ahh93fn3mkpbx7apa04lmcxc637hpxwkivdjbq3nbk3"))))
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
              "-Wno-dev")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'obs-plugins
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output "/lib/obs-plugins"))
              (symlink
               (string-append #$output
                              "/obs-plugins/64bit/obs-multi-rtmp.so")
               (string-append #$output
                              "/lib/obs-plugins/obs-multi-rtmp.so")))))))
    (inputs (list obs qtbase-5))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public obs-gradient-source
  (package
    (name "obs-gradient-source")
    (version "0.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/exeldro/obs-gradient-source")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s1frbax6md9bvlm4zynp9lab9fmh95xk7dq9b2f8q0rhprnb6g6"))))
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
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public obs-shaderfilter
  (package
    (name "obs-shaderfilter")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/exeldro/obs-shaderfilter")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kqa8323gcnyqjcya4ynhwvd38y0xsxvxndzndpmg18q88svyiq8"))))
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
              "-Wno-dev")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'fix-effects
            (lambda _
              (for-each (lambda (directory)
                          (mkdir-p (string-append #$output "/share/obs/obs-plugins/obs-shaderfilter"))
                          (rename-file (string-append #$output "/data/obs-plugins/obs-shaderfilter/" directory)
                                       (string-append #$output "/share/obs/obs-plugins/obs-shaderfilter/" directory)))
                        '("examples" "textures")))))))
    (inputs (list obs qtbase-5))
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
