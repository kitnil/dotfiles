(define-module (packages virtualization)
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
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix gexp)
  #:use-module (gnu packages))

(define %source-dir "/home/oleg/archive/src/looking-glass")

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-pipe port)
      (string-trim-right output #\newline))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(define-public looking-glass-client-next
  (package
    (inherit looking-glass-client)
    (name "looking-glass-client-next")
    (version "B4")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
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

(define-public libvirt-1.6.0
  (package
    (inherit libvirt)
    (version "7.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://libvirt.org/sources/libvirt-"
                           version ".tar.xz"))
       (sha256
        (base32 "0hb1fq0yx41n36c3n1a54b5p37n0m7abs917d76v7aqas03735lg"))
       (patches (search-patches "libvirt-add-install-prefix.patch"))))))
