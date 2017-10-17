;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (wigust packages telegram)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)

  #:use-module (guix build-system cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)

  #:use-module (guix build-system python)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages code)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages emacs)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public mason
  (package
    (name "mason")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mapbox/mason/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15lxq81s1h3zvg9wkqbxpv4r8s711fyzh6mhs24r9j27qccm3mlr"))))
    (build-system trivial-build-system)
    (inputs `(("source" ,source)))
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("gzip" ,gzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((tar (string-append (assoc-ref %build-inputs "tar")
                                   "/bin/tar"))
               (path (string-append (assoc-ref %build-inputs "gzip") "/bin")))
           (setenv "PATH" path)
           (system* tar "xvf" (assoc-ref %build-inputs "source"))
           (chdir (string-append "mason-" ,version))
           (copy-recursively "." (string-append %output "/share/mason"))
           (mkdir-p (string-append (string-append %output "/bin")))
           (symlink (string-append %output "/share/mason/mason")
                    (string-append (string-append %output "/bin/mason")))))))
    (home-page "https://github.com/mapbox/mason")
    (synopsis "Package manager for C/C++ apps")
    (description "Mason is a package manager designed for developers
who package standalone applications and who need complete control over
dependency versions.")
    ;; TODO: no licence
    (license license:expat)))

(define-public python-gyp
  (let ((commit "5e2b3ddde7cda5eb6bc09a5546a76b00e49d888f")
        (revision "1"))
    (package
      (name "python-gyp")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://chromium.googlesource.com/external/gyp.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0fr7nxcrk292djmxzpcjaphnsd123k31gp8jnd91vwknhq6snmv9"))))
      (build-system python-build-system)
      (home-page "https://github.com/Microsoft/GSL")
      (synopsis "Guidelines Support Library")
      (description "The Guideline Support Library (GSL) contains
functions and types that are suggested for use by the C++ Core
Guidelines maintained by the Standard C++ Foundation.")
      (license license:bsd-3))))

(define-public microsoft-gsl
  (let ((commit "4c5fdb541f36211361a05595a3d89fb0afcbec50")
        (revision "1"))
    (package
      (name "microsoft-gsl")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Microsoft/GSL")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1p5i94yqlwr81l3hkj5cpwrnr9pq7j90k9k55v1l53xbinwv4z21"))))
      (build-system cmake-build-system)
      (inputs
       `(("gcc" ,gcc-5)))
      (native-inputs
       `(("catch" ,catch-framework)))
      (arguments
       `(#:parallel-build? #f
         #:out-of-source? #f
         #:make-flags '("LDFLAGS=-fno-strict-aliasing")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'bootstrap
             (lambda _
               (substitute* (find-files "." "\\.[ch]pp$")
                 (("catch/catch\\.hpp")
                  "catch.hpp"))

               ;; unknown pragmas
               (substitute* `(,@(find-files "." "\\.make$")
                              "tests/CMakeLists.txt")
                 (("-Werror") "")))))))
      (home-page "https://github.com/Microsoft/GSL")
      (synopsis "Guidelines Support Library")
      (description "The Guideline Support Library (GSL) contains
functions and types that are suggested for use by the C++ Core
Guidelines maintained by the Standard C++ Foundation.")
      (license license:bsd-3))))
