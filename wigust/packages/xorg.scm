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

(define-public keynav
  (package
    (name "keynav")
    (version "0.20110708.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://http.debian.net/debian/pool/main/k/keynav/keynav_"
             version ".orig.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gizjhji3yspxxxvb90js3z1bv18rbf5phxg8rciixpj3cccff8z"))))
    (build-system gnu-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("libxtst" ,libxtst)
       ("xdotool" ,xdotool)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setenv
           (lambda _
             (setenv "CC" (which "gcc"))))
         (add-after 'unpack 'patch-keynav
           (lambda _
             (substitute* "keynav.c"
               (("xdo_symbol_map") "xdo_get_symbol_map")
               (("xdo_window_setclass") "xdo_set_window_class")
               (("xdo_window_get_active") "xdo_get_window_at_mouse")
               (("xdo_click") "xdo_click_window")
               (("xdo_mouseup") "xdo_mouse_up")
               (("xdo_mousedown") "xdo_mouse_down")
               (("xdo_mousemove") "xdo_move_mouse")
               (("xdo_mousemove_relative") "xdo_move_mouse_relative")
               (("xdo_mouselocation") "xdo_get_mouse_location")
               (("xdo_mouse_wait_for_move_to") "xdo_wait_for_mouse_move_to")
               (("xdo_keysequence_up") "xdo_send_keysequence_window_up")
               (("xdo_keysequence_down") "xdo_send_keysequence_window_down"))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "keynav" (string-append out "/bin"))
               (install-file "keynavrc" (string-append out "/etc"))))))))
    (home-page "https://www.semicomplete.com/projects/keynav/")
    (synopsis "Keyboard-driven mouse cursor mover")
    (description
     "Keynav makes your keyboard a fast mouse cursor mover.  You can move the
cursor to any point on the screen with a few key strokes.  It also simulates
mouse click.  You can do everything mouse can do with a keyboard.")
    (license license:bsd-3)))
