(define-module (packages video)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages chromium)
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
             `(("chromium-embedded-framework" ,chromium-embedded-framework))))
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
      (source (local-file "/home/oleg/src/github.com/lukegb/obs_studio_exporter/obs-studio-exporter.so"))
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
