(define-module (wigust packages games)
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
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression))

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

(define-public libtcod
  (let ((commit "e7c4dbb4a5d133333ea30e02881a47dc93e4d5d1"))
    (package
      (name "libtcod")
      (version (git-version "1.10.2" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libtcod/libtcod.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0azl0v0mlrj0ghm205b0i5rn9xqw1syxb3pdvzbv1nn8bkgp0rnh"))))
      (build-system gnu-build-system)
      (inputs
       `(("sdl" ,sdl2)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("zlib" ,zlib)
         ("python" ,python)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'configure 'bootstrap
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "PATH"
                       (string-append (assoc-ref inputs "python") "/bin" ":"
                                      (getenv "PATH")))
               (chdir "build/autotools") ; autoconf and ./configure
               (substitute* '("get_version.py" "collect_files.py")
                 (("/usr/bin/env python") (which "python3")))
               (invoke "autoreconf" "-vfi"))))))
      (home-page "https://github.com/libtcod/libtcod")
      (synopsis "Libraries for roguelike game development")
      (description "@code{libtcod} is a fast, portable and uncomplicated API
for roguelike developers providing an advanced true color console, input, and
lots of other utilities frequently used in roguelikes.")
      (license license:bsd-3))))

(define-public brogue
  (package
    (name "brogue")
    (version "1.7.5")
    (source (origin
              (method url-fetch)
              (uri "https://sites.google.com/site/broguegame/")
              (file-name (string-append name "-" version "-linux-amd64.tbz2"))
              (sha256
               (base32
                "0i042zb3axjf0cpgpdh8hvfn66dbfizidyvw0iymjk2n760z2kx7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'substitute-gcc
           (lambda _
             (substitute* "Makefile"
               (("gcc") (which "gcc"))
               (("\\$\\(CC\\)") (which "gcc")))))
         ;; TODO: install phase
         ;; (replace 'install
         ;;   (lambda* (#:key outputs #:allow-other-keys)
         ;;     (let ((out (assoc-ref outputs "out")))
         ;;       (with-directory-excursion ))))
         )))
    (inputs
     `(("libtcod" ,libtcod)
       ("ncurses" ,ncurses)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)))))
    (home-page "https://sites.google.com/site/broguegame/")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public tome4-with-addons
  (package
    (inherit tome4)
    (name "tome4-with-addons")
    (version (package-version tome4))
    (inputs
     `(;; You need get and place on your computer the following files:

       ;; https://www.gog.com/game/tales_of_majeyal_ashes_of_urhrok
       ("ashes-urhrok.teaac" ,(local-file "/opt/tome4/rootfs/opt/tome4/game/t-engine4-linux64/game/addons/ashes-urhrok.teaac"))

       ;; https://www.gog.com/game/tales_of_majeyal_forbidden_cults
       ("cults.teaac" ,(local-file "/opt/tome4/rootfs/opt/tome4/game/t-engine4-linux64/game/addons/cults.teaac"))

       ;; https://www.gog.com/game/tales_of_majeyal_embers_of_rage
       ("orcs.teaac" ,(local-file "/opt/tome4/rootfs/opt/tome4/game/t-engine4-linux64/game/addons/orcs.teaac"))
       ,@(package-inputs tome4)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments tome4)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'install-addons
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (for-each (lambda (file)
                           (install-file (assoc-ref inputs file)
                                         (string-append (assoc-ref outputs "out")
                                                        "/share/tome4/game/addons")))
                         '("ashes-urhrok.teaac" "cults.teaac" "orcs.teaac"))))
           (add-before 'install 'my-customizations
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unzip (string-append (assoc-ref inputs "unzip") "/bin/unzip"))
                     (zip (string-append (assoc-ref inputs "zip") "/bin/zip")))
                 (let ((archive (string-append "game/modules/tome-" ,version ".team")))
                   (mkdir-p "mod/class")
                   (system* unzip "-j" archive "mod/class/Game.lua" "-d" "mod/class")
                   (substitute* "mod/class/Game.lua"
                     (("d5990880") "1a1a1a80"))
                   (system* zip archive "mod/class/Game.lua")))))))))))
