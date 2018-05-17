(define-module (wigust packages calamares)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system cmake))

(define-public calamares
  (package
    (name "calamares")
    (version "3.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/calamares/calamares/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1v10fx622prlnnvqw3jaffc5fg6ccwn2hi11vz68g4g98xlif4m2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #f
       #:tests? #f
       #:parallel-build? #f
       #:configure-flags
       (let ((qtwebkit (assoc-ref %build-inputs "qtwebkit")))
         `("-DWEBVIEW_FORCE_WEBKIT:BOOL=ON"
           "-DSKIP_MODULES=\"webview interactiveterminal initramfs \
initramfscfg dracut dracutlukscfg dummyprocess dummypython dummycpp \
dummypythonqt plasmalnf\""
           ,(string-append "-DQt5WebKit_DIR=" qtwebkit "/lib/cmake/Qt5WebKit")
           ,(string-append "-DQt5WebKitWidgets_DIR="
                           qtwebkit "/lib/cmake/Qt5WebKitWidgets")
           "-DCMAKE_VERBOSE_MAKEFILE=True"
           "-DCMAKE_BUILD_TYPE=Release"
           "-DWITH_PYTHONQT:BOOL=ON"))))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kpmcore" ,kpmcore)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("libatasmart" ,libatasmart)
       ("parted" ,parted)
       ("polkit-qt" ,polkit-qt)
       ("python-pyqt" ,python-pyqt)
       ("util-linux" ,util-linux)
       ("qt" ,qt)
       ("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)
       ("qttools" ,qttools)
       ("qtsvg" ,qtsvg)
       ("solid" ,solid)
       ("boost" ,boost)
       ("yaml-cpp" ,yaml-cpp)))
    (home-page "https://calamares.io")
    (synopsis "Distribution-independent installer framework")
    (description
     "This package provides a distribution-independent installer framework.")
    (license #f)))
