;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages pspg)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module ((guix licenses) #:prefix license:))

(define-public pspg
  (package
    (name "pspg")
    (version "2.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/okbob/pspg.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cs0hsrrknl2cv39zzq4wydx5p7095hz18yly572fnniyi4ljbdg"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)
       ("readline" ,readline)))
    (arguments
     '(#:tests? #f))
    (home-page "https://github.com/okbob/pspg/")
    (synopsis "Unix pager designed for work with tables.")
    (description "Unix pager designed for work with tables. Designed for
PostgreSQL, but MySQL is supported too. Now it can be used as CSV or
TSV viewer.")
    (license license:bsd-2)))
