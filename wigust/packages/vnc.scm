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

(define-module (wigust packages vnc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
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
  #:use-module (gnu packages web)
  #:use-module (gnu packages ssh))

#;(define-public tightvnc
  (package
    (name "tightvnc")
    (version "1.3.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/vnc-tight/TightVNC-unix/"
                                  version "/tightvnc-" version "_unixsrc.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gcb118yb5rrf2qbsib5n3iynw9y216w3qiwgpwxsmaydvcbn9dz"))))
    (build-system trivial-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("libjpeg", libjpeg)
       ("imake", imake)
       ("gccmakedep", gccmakedep)
       ("libXmu", libXmu)
       ("libXaw", libXaw)
       ("libXpm", libXpm)
       ("libXp ", libXp )
       ("perl", perl)
       ("xauth", xauth)
       ("openssh", openssh)))
    
    (home-page "http://vnc-tight.sourceforge.net/")
    (synopsis "Virtual network computing server program")
    (description "VNC stands for Virtual Network Computing.  It is, in
essence, a remote display system which allows you to view a computing
`desktop' environment not only on the machine where it is running, but from
anywhere on the Internet and from a wide variety of machine architectures.")
    (license license:gpl2+)))

(define-public tigervnc
  (package
    (name "tigervnc")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append"https://github.com/TigerVNC/tigervnc/archive/v"
                                 version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0h7p86262jmnx81p271n328p50zdb0pgawgj7dn3ampc022ysp7i"))))
    (build-system cmake-build-system)
    (home-page "http://vnc-tight.sourceforge.net/")
    (inputs
     `(("zlib" ,zlib)
       ("libjpeg" ,libjpeg)
       ("imake" ,imake)
       ("libxmu" ,libxmu)
       ("libxaw" ,libxaw)
       ("libxpm" ,libxpm)
       ("libxp" ,libxp)
       ("xauth" ,xauth)
       ("openssh" ,openssh)
       ("xtrans" ,xtrans)
       ;; ("gettext" ,gettext)
       ("xrandr" ,xrandr)))
    (synopsis "Improved version of VNC")
    (description
     "TightVNC is an improved version of VNC, the great free remote-desktop
tool.  The improvements include bandwidth-friendly \"tight\" encoding, file
transfers in the Windows version, enhanced GUI, many bugfixes, and more.")
    (license license:gpl2+)))
