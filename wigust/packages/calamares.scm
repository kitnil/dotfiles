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
  #:use-module (gnu packages python)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix build-system cmake)
  #:use-module (guix build utils))

(define-public boost-1.63
  (package
    (inherit boost)
    (version "1.63.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1c5kzhcqahnic55dxcnw7r80qvwx5sfa2sa97yzv7xjrywljbbmy"))))))

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
       #:build-type "Release"
       #:configure-flags
       (let ((qtwebkit (assoc-ref %build-inputs "qtwebkit"))
             (python (assoc-ref %build-inputs "python"))
             (out (assoc-ref %outputs "out"))
             (boost (assoc-ref %build-inputs "boost")))
         `("-DWEBVIEW_FORCE_WEBKIT:BOOL=ON"
           "-DSKIP_MODULES=\"webview interactiveterminal initramfs \
initramfscfg dracut dracutlukscfg dummyprocess dummypython dummycpp \
dummypythonqt plasmalnf\""
           ,(string-append "-DQt5WebKit_DIR=" qtwebkit "/lib/cmake/Qt5WebKit")
           ,(string-append "-DQt5WebKitWidgets_DIR="
                           qtwebkit "/lib/cmake/Qt5WebKitWidgets")
           ;; ,(string-append "-DCALAMARES_BOOST_PYTHON3_COMPONENT="
           ;;                 (assoc-ref %build-inputs "boost")
           ;;                 "/lib/libboost_python.so")
           "-DCMAKE_VERBOSE_MAKEFILE=True"
           ;; ,(string-append "-DCMAKE_INSTALL_PREFIX=" out)
           ;; "-DCMAKE_BUILD_TYPE=Release"
           ;; ,(string-append "-DPYTHON_LIBRARY="
           ;;                 python "/lib/libpython3.6m.so.1.0")
           ;; ,(string-append "-DPYTHON_INCLUDE_DIR="
           ;;                 python "/include/python3.6m")
           "-DBoost_DEBUG=1"
           "-DBoost_DETAILED_FAILURE_MSG=1"
           ;; "-DWITH_PYTHON:BOOL=ON"
           ;; "-DWITH_PYTHONQT:BOOL=ON"
           ,(string-append "-DPOLKITQT-1_POLICY_FILES_INSTALL_DIR="
                           out "/share/polkit-1/actions")
           "-DCALAMARES_BOOST_PYTHON3_COMPONENT=python"
           ;; "-DLIB_SUFFIX="
           #;"-DSUPPRESS_BOOST_WARNINGS:BOOL=ON"
           #;,(string-append "-DBoost_INCLUDE_DIRS=" boost "/include")
           ;; ,(string-append "-DBoost_LIBRARY_DIR=" boost "/lib")
           #;"-DBoost_NO_BOOST_CMAKE=ON"))
       #:modules ((ice-9 match)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-partutils
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/modules/partition/core/PartUtils.cpp"
               (("\"os-prober\"") (string-append "\"" (which "os-prober")
                                                 "\"")))
             ;; (setenv "BOOST_LIB_SUFFIX" "")

             ;; /tmp/guix-build-calamares-3.1.10.drv-0/calamares-3.1.10/CMakeModules/BoostPython3.cmake
             ;; (setenv "CALAMARES_BOOST_PYTHON3_COMPONENT"
             ;;         "libboost_python.so")

             (setenv "BOOST_LIBRARYDIR"
                     (string-append (assoc-ref %build-inputs "boost") "/lib"))
             ;; (setenv "BOOST_ROOT" (assoc-ref %build-inputs "boost"))
             ;; (setenv "BOOST_LIBRARYDIR"
             ;;         (string-append (assoc-ref %build-inputs "boost")
             ;;                        "/lib"))
             ;; Required to create a package registry file.
             (setenv "HOME" (getcwd))))
         (add-before 'install 'patch-cmakelists
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/polkit-1/actions")))))
         ;; (add-after 'configure 'fail
         ;;     (lambda _ #f))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (qt (delete #f
                               (map (lambda (input)
                                      (match input
                                        ((name package)
                                         (if (string-prefix? "qt" name)
                                             name
                                             #f))
                                        (_ #f)))
                                    inputs))))
               (wrap-program (string-append out "/bin/calamares")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins/"))
                         (append qt '("kpmcore")))))
               #t))))))
    (inputs
     `(("os-prober" ,os-prober)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kiconthemes" ,kiconthemes)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kpmcore" ,kpmcore)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("libatasmart" ,libatasmart)
       ("parted" ,parted)
       ("polkit-qt" ,polkit-qt)
       ;; ("python-pyqt" ,python-pyqt)
       ("util-linux" ,util-linux)
       ("qt" ,qt)
       ("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)
       ("qttools" ,qttools)
       ("qtsvg" ,qtsvg)
       ("solid" ,solid)
       ("boost" ,boost-1.63)
       ("python" ,python)
       ("yaml-cpp" ,yaml-cpp)))
    (home-page "https://calamares.io")
    (synopsis "Distribution-independent installer framework")
    (description
     "This package provides a distribution-independent installer framework.")
    (license #f)))
