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

(define-module (wigust packages pulseaudio)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages pulseaudio))

(define-public pulsemixer-emacs-keybindings
  (package
    (inherit pulsemixer)
    (name "pulsemixer-emacs-keybindings")
    (source (origin
              (inherit (package-source pulsemixer))
              (patches (search-patches "pulsemixer-emacs-keybindings.patch"))))
    (description "Pulsemixer is a PulseAudio mixer with command-line and
curses-style interfaces with Emacs keybindings.")))
