(define-module (packages virtualization)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public looking-glass-client-next
  (package
    (inherit looking-glass-client)
    (name "looking-glass-client-next")
    (version "B4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/gnif/LookingGlass")
                           (commit version)
                           (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fwmz0l1dcfwklgvxmv0galgj2q3nss90kc3jwgf6n80x27rsnhf"))))
    (inputs
     `(("binutils" ,binutils) ;for libbfd
       ("libiberty" ,libiberty)
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
       ,@(package-inputs looking-glass-client)))
    (arguments
     `(#:validate-runpath? #f
       ;; #:configure-flags '("-DENABLE_X11=OFF") ;failed to build with X11
       ,@(substitute-keyword-arguments (package-arguments looking-glass-client)
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
