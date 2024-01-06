(define-module (packages video)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages chromium)
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

(define-public obs-with-ecf
  (package
    (inherit obs)
    (inputs
     (append (package-inputs obs)
             `(("chromium-embedded-framework" ,chromium-embedded-framework)
               ("coreutils" ,coreutils)
               ("bash" ,bash))))
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build cmake-build-system))
      #:configure-flags
      #~(list (string-append "-DOBS_VERSION_OVERRIDE=" #$(package-version obs))
              "-DENABLE_UNIT_TESTS=ON"
              "-DENABLE_NEW_MPEGTS_OUTPUT=OFF"
              "-DENABLE_AJA=OFF"
              "-DBUILD_BROWSER=ON"
              "-DCEF_ROOT_DIR=../source/cef")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-executable
             (lambda* _
               (let ((plugin-path (getenv "QT_PLUGIN_PATH")))
                 (wrap-program (string-append #$output "/bin/obs")
                   `("QT_PLUGIN_PATH" ":" prefix (,plugin-path))
                   `("LD_LIBRARY_PATH" ":" prefix (,(string-append #$(this-package-input vlc) "/lib")))))))
          (add-before 'configure 'add-cef
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "libcef" #$(this-package-input "chromium-embedded-framework"))
              (copy-file #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/modules/packages/run.sh")
                         "run.sh")
              (setenv "PATH"
                      (string-append
                       #$(this-package-input "coreutils") "/bin"
                       ":" (getenv "PATH")))
              (invoke (string-append #$(this-package-input "bash") "/bin/bash")
                      #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/modules/packages/run.sh"
                                    #:recursive? #t))))
          (add-after 'install 'symlink
            ;; Required for lib/obs-plugins/obs-browser.so
            (lambda* (#:key outputs #:allow-other-keys)
              (symlink (string-append #$output "/lib/libobs-frontend-api.so.0")
                       (string-append #$output "/lib/obs-plugins/libobs-frontend-api.so.0"))
              (symlink (string-append #$output "/lib/libobs.so.0")
                       (string-append #$output "/lib/obs-plugins/libobs.so.0")))))))))
