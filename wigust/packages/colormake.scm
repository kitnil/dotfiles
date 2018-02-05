;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages colormake)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config))

(define-public colormake
  (package
    (name "colormake")
    (version "0.9.20140503")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/pagekite/Colormake/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08ldss9zd8ls6bjahvxhffpsjcysifr720yf3jz9db2mlklzmyd3"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)
       ("perl" ,perl)
       ("bash" ,bash)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   ;; bootstrap
                   (setenv "PATH"
                           (string-append
                            (assoc-ref %build-inputs "tar") "/bin" ":"
                            (assoc-ref %build-inputs "gzip") "/bin"))
                   (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                   (chdir (string-append (string-capitalize ,name)
                                         "-" ,version))
                   ;; patch-shebangs
                   (patch-shebang  "colormake.pl"
                                   (list (string-append (assoc-ref %build-inputs
                                                                   "perl")
                                                        "/bin")))
                   ;; install
                   (let* ((out (assoc-ref %outputs "out"))
                          (bin (string-append out "/bin"))
                          (install-files (lambda (files directory)
                                           (for-each (lambda (file)
                                                       (install-file file
                                                                     directory))
                                                     files))))
                     (substitute* "colormake"
                       (("/bin/bash")
                        (string-append (assoc-ref %build-inputs "bash")
                                       "/bin/sh"))
                       (("colormake\\.pl")
                        (string-append bin "/colormake.pl")))
                     (install-file "colormake.1"
                                   (string-append out "/share/doc/man/man1"))
                     (install-files '("colormake" "colormake-short" "clmake"
                                      "clmake-short" "colormake.pl")
                                    bin)
                     (install-files '("AUTHORS" "BUGS" "ChangeLog" "README")
                                    (string-append out "/share/doc"))))))
    (home-page "http://bre.klaki.net/programs/colormake/")
    (synopsis "Simple wrapper around make to make it's output more readable")
    (description "This package provides a simple wrapper around make to make
it's output more readable.")
    (license gpl2+)))
