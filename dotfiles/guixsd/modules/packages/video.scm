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

(define-module (packages video)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages python)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public y2rss
  (let ((commit "d5bc8173028d1594d6c7575e3f7c309553403074")
        (revision "1"))
    (package
      (name "y2rss")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AN3223/scripts.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19xlllvgaivaxjc43vxi20cc937pi2vhgsg78iplbrilndy5zssy"))))
      (build-system trivial-build-system)
      (inputs
       `(("python" ,python)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "y2rss"
             (("/usr/bin/env python3")
              (string-append (assoc-ref %build-inputs "python")
                             "/bin/python3")))
           (install-file "y2rss" (string-append %output "/bin"))
           #t)))
      (home-page "https://github.com/AN3223/scripts/blob/master/y2rss")
      (synopsis "Convert URL to a YouTube channel/playlist RSS")
      (description "This script takes a URL argument to a YouTube
channel/playlist and returns a link to the corresponding RSS feed.")
      (license #f))))
