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

(define-module (wigust packages git-splits)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash))

(define-public git-splits
  (package
    (name "git-splits")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ajdruff/git-splits.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14cxcivr10aq5399hf102sbp4gimww7jqr669mbxfly1w7y4yv6y"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-file (string-append (assoc-ref %build-inputs "source") "/git-splits")
                    "git-splits")
         (substitute* "git-splits"
           (("/usr/bin/env bash")
            (string-append (assoc-ref %build-inputs "bash") "/bin/bash")))
         (install-file "git-splits" (string-append %output "/bin"))
         #t)))
    (home-page "https://github.com/ajdruff/git-splits/")
    (synopsis "Extracts multiple directories of a git repo into a new branch")
    (description "git-splits - Extracts directories into a new branch
with re-written history containing only those directories.")
    (license license:expat)))
