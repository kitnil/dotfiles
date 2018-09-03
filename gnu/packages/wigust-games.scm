(define-module (gnu packages wigust-games)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages games)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system trivial))

(define-public stb
  (let ((commit "9d9f75eb682dd98b34de08bb5c489c6c561c9fa6")
        (revision "1"))
    (package
      (name "stb")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://github.com/nothings/stb.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0q84bl6ai2mrcywrynvqvvlr6dpyafx33j3xaz6n38z5gi8lcmzx"))))
      (build-system trivial-build-system)
      (inputs `(("source" ,source)))
      (arguments
       `(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (for-each (lambda (file)
                       (install-file file (string-append %output
                                                         "/include/stb")))
                     (find-files (assoc-ref %build-inputs "source")
                                 "\\.h$")))))
      (home-page "https://github.com/nothings/stb")
      (synopsis "stb single-file public domain libraries for C/C++")
      (description "stb single-file public domain libraries for C/C++")
      (license license:expat))))

(define-public angband-nonfree
  ;; TODO: Sound, X11.
  (package
    (inherit angband)
    (name "angband-nonfree")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://rephial.org/downloads/4.1/"
                           "angband-" version ".tar.gz"))
       (sha256
        (base32
         "0vhvzbrm6hwca2yp02pg2vzg4c5yf65whg0bmjbalmhy1r4kw51x"))))
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)))
       ("freetype" ,freetype)
       ("libx11" ,libx11)
       ("perl" ,perl)
       ,@(package-inputs angband)))
    (arguments
     `(#:tests?
       #f
       #:configure-flags
       (list (string-append "--bindir=" %output "/bin")
             (string-append "--sysconfdir=" %output "/share/angband")
             "--enable-sdl" "CFLAGS=-fgnu89-inline" "--enable-sdl-mixer")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen.sh
           (lambda _
             (zero? (system* "sh" "autogen.sh"))
             (substitute* "src/main-sdl.c" (("SDL_ttf.h") "SDL/SDL_ttf.h"))
             (substitute* "src/main-sdl.c" (("SDL_image.h") "SDL/SDL_image.h"))
             #t)))))))
