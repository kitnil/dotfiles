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

(define-module (wigust packages inxi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages admin))

(define-public inxi
  ;; TODO: Add more inputs.
  ;; See <https://git.archlinux.org/svntogit/community.git/plain/trunk/PKGBUILD?h=packages/inxi>.
  (let ((commit "c934578ffb4f920cb04c91305a54dbdc4aa99d80")
        (revision ""))
      (package
    (name "inxi")
    (version (string-append "2.3.56" revision "."
                            (string-take commit 7)))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/smxi/inxi"
                           "/raw/" commit "/inxi.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cmb95fw4jf5fqxmivwxy63sqyl7jnb3v7sbqqhafx0iwxgxi77h"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)))
    (native-inputs
     `(("coreutils" ,coreutils)
       ("gawk" ,gawk)
       ("grep" ,grep)
       ("pciutils" ,pciutils)
       ("procps" ,procps)
       ("sed" ,sed)
       ("tar" ,tar)
       ("gzip" ,gzip)
       ("xdpyinfo" ,xdpyinfo)
       ("mesa-utils" ,mesa-utils)
       ("dmidecode" ,dmidecode)))
    (arguments
     `(#:modules
       ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "tar") "/bin" ":"
                         (assoc-ref %build-inputs "gzip") "/bin"))
         (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
         ;; Many commands (like glxinfo) better be searched as words.
         ;; See SRFI-115.
         (with-fluids ((%default-port-encoding #f))
           (substitute* "inxi"
             (("/usr/bin/env bash")
              (string-append (assoc-ref %build-inputs "bash") "/bin/bash"))
             (("xdpyinfo")
              (string-append (assoc-ref %build-inputs "xdpyinfo")
                             "/bin/xdpyinfo"))
             (("glxinfo")
              (string-append (assoc-ref %build-inputs "mesa-utils")
                             "/bin/glxinfo"))
             (("dmidecode")
              (string-append (assoc-ref %build-inputs "dmidecode")
                             "/bin/dmidecode"))))
         (install-file "inxi" (string-append %output "/bin"))
         (install-file "inxi.1.gz"
                       (string-append %output "/share/doc/man/man1"))
         #t)))
    (home-page "http://inxi.org")
    (synopsis "Script to get system information")
    (description
     "This package provides a script to get system information.")
    (license license:gpl3+))))
