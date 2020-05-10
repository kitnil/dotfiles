;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages dotfiles)
  #:use-module (guix build utils)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module ((guix licenses) #:prefix license:))

(define-public dotfiles
  (let ((commit "e22e16b9aa1cdabe0c7f75aed46e134d0d4a613e"))
    (package
      (name "dotfiles")
      (version (git-version "2.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://cgit.duckdns.org/git/wigust/dotfiles")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0s8099j25x36s5pzj90pj103nbpj2dq72rdykvv8842wxiq36q2r"))))
      (build-system trivial-build-system)
      (inputs
       `(("bash" ,bash)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((bin (string-append %output "/bin"))
                 (dotfiles (string-append %output "/share/dotfiles"))
                 (executable (string-append %output "/bin/dotfiles")))
             (copy-recursively (assoc-ref %build-inputs "source") ".")
             (substitute* "bin/executable_dotfiles"
               (("@GUIX@") dotfiles)
               (("/bin/sh")
                (string-append (assoc-ref %build-inputs "bash") "/bin/bash")))

             (mkdir-p dotfiles)
             (copy-recursively "." dotfiles)

             (mkdir-p bin)
             (copy-file "bin/executable_dotfiles" executable)
             (chmod executable #o555))
           #t)))
      (home-page "https://gitlab.com/wigust/dotfiles/")
      (license license:gpl3+)
      (synopsis "WiGust dotfiles")
      (description "This package provides wigust dotfiles which could
be installed with chezmoi."))))
