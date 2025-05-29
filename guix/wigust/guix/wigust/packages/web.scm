;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages web)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public homer
  (package
    (name "homer")
    (version "22.02.2")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://github.com/bastienwirtz/homer/releases/download/v"
             version "/homer.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "04hybcakdxip1adhb0hhzgczvi47z8a9qaijvvsb8rpphj82x2fz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (data (string-append out "/share/homer")))
           (mkdir-p data)
           (copy-recursively (assoc-ref %build-inputs "source") data)
           (symlink "/etc/homer/config.yml"
                    (string-append data "/assets/config.yml"))
           #t))))
    (home-page "https://github.com/bastienwirtz/homer/")
    (synopsis "Static homepage for server to keep your services on hand")
    (description "Homer is a full static html/js dashboard, generated from the
source in /src using webpack.  It's meant to be served by an HTTP server.")
    (license license:expat)))
