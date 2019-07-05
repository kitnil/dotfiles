;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages admin)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages cairo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg))

(define-public pscircle
  (package
    (name "pscircle")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/mildlyparallel/pscircle/-/archive/v"
             version "/pscircle-v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1k757yf2bmgfrjd417l6kpcf83hlvi0z1791vz967mwcklrsb3fj"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)))
    (home-page "https://gitlab.com/mildlyparallel/pscircle")
    (synopsis "Visualize Linux processes in a form of radial tree")
    (description
     "@code{pscircle} visualizes Linux processes in a form of radial tree")
    (license license:gpl2+)))
