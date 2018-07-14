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

(define-module (wigust packages cdrom)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (lgpl2.1+ gpl2 gpl2+ gpl3+ cddl1.0))
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages python)
  #:use-module (gnu packages image)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages cdrom))

(define-public cdrkit
  (package
    (name "cdrkit")
    (version "1.1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://cdrkit.org/releases/" name ".tar.gz"))
              (sha256
               (base32
                "1nj7iv3xrq600i37na9a5idd718piiiqbs4zxvpjs66cdrsk1h6i"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("bzip2" ,bzip2)
       ("libcap" ,libcap)
       ("perl" ,perl)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'old-cdrecord
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion (string-append (assoc-ref outputs "out")
                                                      "/bin")
               (symlink "genisoimage" "mkisofs")
               (symlink "wodim" "cdrecord")))))))
    (home-page "http://cdrkit.org/")
    (synopsis "Portable command-line CD/DVD recorder software, mostly
compatible with cdrtools")
    (description "Cdrkit is a suite of programs for recording CDs and DVDs,
blanking CD-RW media, creating ISO-9660 filesystem images, extracting audio CD
data, and more.  The programs included in the cdrkit package were originally
derived from several sources, most notably mkisofs by Eric Youngdale and
others, cdda2wav by Heiko Eissfeldt, and cdrecord by Jörg Schilling.  However,
cdrkit is not affiliated with any of these authors; it is now an independent
project.")
    (license gpl2+)))
