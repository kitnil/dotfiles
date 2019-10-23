;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public libxss
  (package
    (name "libxss")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/lib/libXss-"
             version
             ".tar.bz2"))
       (sha256
        (base32
         "0j89cnb06g8x79wmmnwzykgkkfdhin9j7hjpvsxwlr3fz1wmjvf0"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("libext" ,libxext)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("scrnsaverproto" ,scrnsaverproto)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X11 Screen Saver extension library")
    (description "libXss provides an X Window System client interface to the
MIT-SCREEN-SAVER extension to the X11 protocol.")
    (license license:x11)))

(define-public tabbed
  (package
    (name "tabbed")
    (version "0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.suckless.org/tabbed")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gyhfvim1ckf4g78ysl6l4k9nckipnrp1338zh9klncszfraakj3"))))
    (build-system gnu-build-system)
    (inputs
     `(("xorgproto" ,xorgproto)
       ("libx11" ,libx11)))
    (arguments
       `(#:tests? #f ;no tests
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX=" %output))
         #:phases
         (modify-phases %standard-phases
            ;; no configure script
           (delete 'configure))))
    (home-page "https://tools.suckless.org/tabbed/")
    (synopsis "Frontend to xembed aware applications")
    (description "Simple generic tabbed frontend to xembed aware applications,
originally designed for surf but also usable with many other applications,
i.e. @code{st}, @code{uzbl}, @code{urxvt} and @code{xterm}.")
    (license license:isc)))

(define-public xterm-my
  (package
    (inherit xterm)
    (name "xterm-my")
    (arguments
     (substitute-keyword-arguments
         (package-arguments xterm)
       ((#:configure-flags flags)
        ;; XXX: Broken flags: "--enable-double-buffer" "--with-utempter"
        `(cons "--enable-exec-xterm" ,flags))))))
