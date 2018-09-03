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

(define-module (gnu packages wigust-image-viewers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages))

(define-public nomacs
  (package
    (name "nomacs")
    (version "3.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/nomacs/nomacs/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ib8w3ax9zmbx99s1rzj9iszwpmcwc6i3jx0p8pxp418gx8d2fpl"))))
    (build-system cmake-build-system)
    (inputs
     `(;; opencv
       ("qtbase" ,qtbase)
       ("exiv2" ,exiv2)
       ("libraw" ,libraw)
       ("python" ,python)))
    (home-page "https://nomacs.org/")
    (synopsis "Image viewer with capability of syncing multiple instances")
    (description "@code{nomacs} is small image viewer, fast and able to handle
the most common image formats.  Additionally it is possible to synchronize
multiple viewers.  A synchronization of viewers running on the same computer
or via LAN is possible.  It allows one to compare images and spot the
differences (e.g.  schemes of architects to show the progress).")
    (license license:gpl3+)))
