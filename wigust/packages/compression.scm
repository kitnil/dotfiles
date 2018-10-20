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

(define-module (wigust packages compression)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (last)))

(define-public unrar
  (package
    (name "unrar")
    (version "5.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.rarlab.com/rar/unrarsrc-" version ".tar.gz"))
              (sha256
               (base32
                "0blpl78grcr6d9wwwxhd5343i223972acnzhp7lhm12xfh5fg465"))))
    (build-system gnu-build-system)
    (home-page "http://www.rarlab.com/rar_add.htm")
    (synopsis "RAR archive extraction tool")
    (description "Unrar is a simple command-line program to list and extract
RAR archives.")
    (license license:gpl2+)))
