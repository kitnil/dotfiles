;;; GNU Guix --- Functional package management for GNU
;;; Copyright © Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (packages majordomo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

(define-public majordomo-ca
  (package
    (name "majordomo-ca")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cgit.duckdns.org"
                    "/guix/guix-majordomo"
                    "/plain/majordomo/packages/Majordomo_LLC_Root_CA.crt"))
              (sha256
               (base32
                "1nyj8sns0vfm51bky3cwf2cvx8fqw1nyb678r9v8wrf75ssbzwd0"))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/etc/ssl/certs"))
         (copy-file (assoc-ref %build-inputs "source")
                    (string-append %output "/etc/ssl/certs/Majordomo_LLC_Root_CA.pem"))
         #t)))
    (build-system trivial-build-system)
    (home-page "https://www.majordomo.ru/")
    (synopsis "Majordomo CA")
    (description "This package provides a Majordomo CA")
    (license license:gpl3+)))
