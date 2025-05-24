;;; Dotfiles --- My dotfiles.
;;; Copyright © 2025 Oleg Pykhalov <go.wigust@gmail.com>
;;;
;;; This file is part of Guile IHS.
;;;
;;; Guile IHS is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; Guile IHS is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile IHS.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-module (wugi packages dotfiles)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public dotfiles
  (package
    (name "dotfiles")
    (version "1.0.0")
    (source #f)
    (build-system gnu-build-system)
    (home-page "https://wugi.info/")
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-3.0-latest)))
    (synopsis "Dotfiles")
    (description
     "This package provides dotfiles")
    (license license:gpl3+)))
