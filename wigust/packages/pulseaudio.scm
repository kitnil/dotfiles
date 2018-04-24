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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages ncurses)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial))

(define-public pulsemixer-emacs-keybindings
  (package
    (inherit pulsemixer)
    (name "pulsemixer-emacs-keybindings")
    (source (origin
              (inherit (package-source pulsemixer))
              (patches (search-patches "pulsemixer-emacs-keybindings.patch"))))
    (description "Pulsemixer is a PulseAudio mixer with command-line and
curses-style interfaces with Emacs keybindings.")))

(define-public ncpamixer
  (package
    (name "ncpamixer")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fulhax/ncpamixer/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1d1vna5bs4dk6jz28slxs4d5d21fm2h39vbljx6np1gzhsnl7c7p"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("pulseaudio" ,pulseaudio)
       ("ncurses" ,ncurses)))
    (home-page "https://github.com/fulhax/ncpamixer")
    (synopsis "ncurses PulseAudio Mixer")
    (description
     "Command-line ncurses mixer for PulseAudio inspired by pavucontrol.")
    (license license:x11)))

(define-public pulseaudio-alsa
  (package
    (name "pulseaudio-alsa")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (etc (string-append out "/etc")))
           (mkdir-p etc)
           (call-with-output-file (string-append etc "/asound.conf")
             (lambda (port)
               (display "# Use PulseAudio by default
pcm.!default {
  type pulse
  fallback \"sysdefault\"
  hint {
    show on
    description \"Default ALSA Output (currently PulseAudio Sound Server)\"
  }
}

ctl.!default {
  type pulse
  fallback \"sysdefault\"
}
"
                        port)))
           #t))))
    (synopsis "ALSA Configuration for PulseAudio")
    (description "This package provides a @file{asound.conf} file to configure
ALSA for PulseAudio.")
    (home-page #f)
    (license license:gpl3+)))
