(define-module (packages virtualization)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define-public libxpresent
  (package
    (name "libxpresent")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri "mirror://xorg/individual/lib/libXpresent-1.0.0.tar.bz2")
              (sha256
               (base32
                "12kvvar3ihf6sw49h6ywfdiwmb8i1gh8wasg1zhzp6hs2hay06n1"))))
    (inputs
     (list libx11 xorgproto pkg-config libxext libxfixes libxrandr))
    (build-system gnu-build-system)
    (home-page "https://gitlab.freedesktop.org/xorg/lib/libxpresent")
    (synopsis "Xlib-compatible API for the Present extension")
    (description "This package provides a Xlib-based library for the X Present
Extension.")
    (license license:x11)))

(define-public looking-glass-client-next
  (package
    (inherit looking-glass-client)
    (name "looking-glass-client-next")
    (version "B5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnif/LookingGlass")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09mn544x5hg1z31l92ksk7fi7yj9r8xdk0dcl9fk56ivcr452ylm"))))
    (inputs
     `(("libiberty" ,libiberty)
       ("zlib" ,zlib)
       ("zlib:static" ,zlib "static")
       ("wayland-protocols" ,wayland-protocols)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxi" ,libxi)
       ("libglvnd" ,libglvnd)
       ("gmp" ,gmp)
       ("guile" ,guile-3.0) ; for the wrapper script
       ("nettle" ,nettle)
       ("libx11" ,libx11)
       ("libXfixes" ,libxfixes)
       ("libxss" ,libxscrnsaver)
       ("libxinerama" ,libxinerama)
       ("freetype" ,freetype)
       ("libxkbcommon" ,libxkbcommon)
       ("libxcursor" ,libxcursor)
       ("libxpresent" ,libxpresent)
       ("libxrandr" ,libxrandr)
       ,@(package-inputs looking-glass-client)))
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments looking-glass-client)
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'add-missing-include)
               (add-after 'install 'wrapper
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (wrap-program
                       (string-append (assoc-ref outputs "out")
                                      "/bin/looking-glass-client")
                     `("LD_LIBRARY_PATH" ":" prefix
                       ,(map (lambda (name)
                               (let ((input (assoc-ref inputs name)))
                                 (string-append input "/lib")))
                             '("gmp"
                               "libxi"
                               "nettle"
                               "mesa"
                               "wayland"
                               "fontconfig"
                               "freetype"
                               "libx11"
                               "libXfixes"
                               "libxss"
                               "libxinerama"))))
                   #t)))))))))
