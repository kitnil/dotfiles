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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages file)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages man)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages web))

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
