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

(define-module (wigust packages feh)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public feh-fixed
  (package
    (inherit feh)
    (name "feh-fixed")
    (source
     (origin
       (inherit (package-source feh))
       (patches (append (search-patches "feh-issue-480.patch")
                        (origin-patches (package-source feh))))))))
