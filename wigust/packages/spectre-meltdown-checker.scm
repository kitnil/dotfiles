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

(define-module (wigust packages spectre-meltdown-checker)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public spectre-meltdown-checker
  (package
    (name "spectre-meltdown-checker")
    (version "0.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/speed47/spectre-meltdown-checker"
                           "/archive/" "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0dashyjzjhjvg6gw3vj8fx4pkil9262xg16lgqc7vb22d84g5952"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("bash" ,bash)
       ("gzip" ,gzip)
       ("perl" ,perl)
       ("tar" ,tar)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         ;; bootstrap
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "tar") "/bin" ":"
                         (assoc-ref %build-inputs "gzip") "/bin"))
         (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
         (chdir (string-append ,name "-" ,version))
         (substitute* "spectre-meltdown-checker.sh"
           (("/bin/sh")
            (string-append (assoc-ref %build-inputs "bash") "/bin/sh")))
         ;; install
         (let ((out (assoc-ref %outputs "out")))
           (install-file "spectre-meltdown-checker.sh"
                         (string-append out "/bin"))
           (for-each (lambda (file)
                       (install-file file (string-append out "/share/doc")))
                     '("LICENSE" "README.md"))))))
    (home-page "https://github.com/speed47/spectre-meltdown-checker/")
    (synopsis "Spectre & Meltdown vulnerability/mitigation checker for Linux")
    (description "A simple shell script to tell if your Linux installation is
vulnerable against the 3 “speculative execution” CVEs that were made public
early 2018.

Without options, it'll inspect your currently running kernel.  You can also
specify a kernel image on the command line, if you'd like to inspect a kernel
you're not running.

The script will do its best to detect mitigations, including backported
non-vanilla patches, regardless of the advertised kernel version number.")
    (license license:gpl3+)))
