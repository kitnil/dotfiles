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

(define-module (wigust packages sipcalc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages admin))

(define-public sipcalc
  (package
    (name "sipcalc")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.routemeister.net/projects"
                           "/sipcalc/files/sipcalc" "-" version ".tar.gz"))
       (sha256
        (base32
         "0mv3wndj4z2bsshh2k8d5sy3j8wxzgf8mzmmkvj1k8gpcz37dm6g"))))
    (build-system gnu-build-system)
    (home-page "http://www.routemeister.net/projects/sipcalc/")
    (synopsis "Advanced console based IP subnet calculator")
    (description
     "Sipcalc is an advanced console-based IP subnet calculator.  It can take
multiple forms of input (IPv4/IPv6/interface/hostname) and output a multitude
of information about a given subnet.

Features include:

@itemize @bullet
@item IPv4
@itemize
@item Retrieving of address information from interfaces.
@item Classfull and CIDR output.
@item Multiple address and netmask input and output formats (dotted quad, hex,
number of bits).
@item Output of broadcast address, network class, Cisco wildcard,
hosts/range, network range.
@item The ability to split a network based on a smaller netmask, now also with
recursive runs on the generated subnets.  (also IPv6)
@end itemize
@item IPv6
@itemize
@item Compressed and expanded input and output addresses.
@item Standard IPv6 network output.
@item v4 in v6 output.
@item Reverse DNS address generation.
@end itemize
@end itemize\n")
    (license license:bsd-3)))
