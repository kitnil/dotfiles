(define-module (packages video)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (packages chromium)
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
           "1mllfrlxryy979k72jdmmz1ri3cgy66p2gchpn37qh5j910zmqrc"))))
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
