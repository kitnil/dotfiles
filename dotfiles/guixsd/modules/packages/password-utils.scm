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

(define-module (packages password-utils)
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

(define-public vault
  (package
    (name "vault")
    (version "1.9.3")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://releases.hashicorp.com/vault/"
                                  version "/vault_" version "_linux_amd64.zip"))
              (sha256
               (base32
                "0fc6a8d5jm17sk7ppqs8adhw5adbcvm218vbzq02ipxlbwj9y18n"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (mkdir-p (string-append %output "/bin"))
         (install-file (string-append (assoc-ref %build-inputs "source")
                                      "/vault")
                       (string-append %output "/bin")))))
    (home-page "https://www.vaultproject.io/")
    (synopsis "A tool for managing secrets, this binary includes the UI")
    (description "A tool for managing secrets, this binary includes the UI")
    (license license:mpl2.0)))

(define-public mintotp
  (package
    (name "mintotp")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/susam/mintotp")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xryg3s839r6cg0x0lk3aj9bcwaqya9cfr4v0pp7x2zc60bbb5i7"))))
    (build-system python-build-system)
    (home-page "https://github.com/susam/mintotp/")
    (synopsis "Minimal TOTP generator written in Python")
    (description "This package provides a minimal TOTP generator written in Python.")
    (license license:expat)))
