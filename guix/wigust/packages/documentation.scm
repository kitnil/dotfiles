;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages documentation)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial))

(define-public slides-concise-gnu-bash
  (package
    (name "slides-concise-gnu-bash")
    (version "2017")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://talk.jpnc.info/bash_lnfw_"
                                  version ".pdf"))
              (sha256
               (base32
                "1v8nn3p7qiibsmbigdcv8q40pgsq6s8v63193f7qq5y2yhrqml7a"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin (use-modules (guix build utils))
              (let ((install-dir (string-append %output "/share/doc/slides-concise-gnu-bash")))
                (mkdir-p install-dir)
                (copy-file (assoc-ref %build-inputs "source") (string-append install-dir "/slides-concise-gnu-bash.pdf")))
              #t)))
    (home-page "http://talk.jpnc.info/")
    (synopsis "Introduction to Bash advances usage")
    (description "This package provides slides for a presention Introduction
to Bash advances usage.")
    (license #f)))

(define-public documentation-arcconf
  (package
    (name "documentation-arcconf")
    (version "3_00_23484_ug")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.adaptec.com/pdfs/user_guides/microsemi_cli_smarthba_smartraid_v"
                                  version ".pdf"))
              (sha256
               (base32
                "0x2bzi1ywpin8504ra9zlzh5aij15gqgfmjj1b5kylaap6vb92xb"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin (use-modules (guix build utils))
              (let ((install-dir (string-append %output "/share/doc/arcconf")))
                (mkdir-p install-dir)
                (copy-file (assoc-ref %build-inputs "source") (string-append install-dir "/arcconf.pdf")))
              #t)))
    (home-page "http://download.adaptec.com/pdfs/user_guides/")
    (synopsis "ARCCONF Command Line Utility")
    (description "Microsemi Smart Storage Controllers User's Guide.")
    (license #f)))
