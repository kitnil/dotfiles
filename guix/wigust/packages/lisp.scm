;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020, 2021, 2022, 2023, 2024 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages lisp)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages m4)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-syntax define-stumpwm-next-package
  (lambda (s)
    "Convert stumpwm-checkout to stumpwm-next package."
    (with-ellipsis
     :::
     (syntax-case s ()
       ((_ var-name pkg)
        #'(begin
            (define-public var-name
              (package
                (inherit pkg)
                (name (match (string-split (package-name pkg) #\-)
                        (("sbcl" "stumpwm" "checkout" package ...)
                         (string-join (append '("sbcl" "stumpwm" "next") package) "-"))
                        (a (string-join a "-"))))
                (inputs
                 `(("stumpwm" ,stumpwm-next "lib")
                   ,@(alist-delete "stumpwm" (package-inputs pkg))))))))))))

(define-public stumpwm-checkout
  (let ((commit "bd9151cda21313a928a1cf410a7c608bcc8459e8"))
    (package
      (inherit stumpwm)
      (name (string-append (package-name stumpwm)))
      (version (git-version (package-version stumpwm) "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wigust/stumpwm.git")
               (commit commit)))
         (file-name (git-file-name (package-name stumpwm) version))
         (sha256
          (base32
           "0qrp9ywn2qb1f1jq7wr7s8di64isf5h4ld91yqh2nb8kfqdf8jsl"))))
      (inputs
       `(("sbcl-alexandria" ,sbcl-alexandria)
         ("sbcl-fiasco", sbcl-fiasco)
         ("dbus" ,dbus)
         ,@(package-inputs stumpwm)))
      (arguments
       (substitute-keyword-arguments
           (package-arguments stumpwm)
           ((#:phases phases)
            `(modify-phases ,phases
               (replace 'create-desktop-file
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (xsessions (string-append out "/share/xsessions"))
                          (dbus (assoc-ref inputs "dbus")))
                     (mkdir-p xsessions)
                     (with-output-to-file
                         (string-append xsessions "/stumpwm.desktop")
                       (lambda ()
                         (display "[Desktop Entry]")
                         (newline)
                         (display "Name=stumpwm")
                         (newline)
                         (display "Comment=The Stump Window Manager")
                         (newline)
                         (format #t "Exec=~a/bin/dbus-launch ~a/bin/stumpwm~%" dbus out)
                         (format #t "TryExec=~a/bin/dbus-launch ~a/bin/stumpwm~%" dbus out)
                         (display "Icon=")
                         (newline)
                         (display "Type=Application")
                         (newline)))
                     #t))))))))))

(define-public stumpwm-next
  (let ((commit "ca3d065186b52aefa70a7cf5fce7684f0979d946"))
    (package
      (inherit stumpwm-checkout)
      (name "stumpwm-next")
      (version (git-version (package-version stumpwm) "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wigust/stumpwm.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "119qsh7gj8bp8gcc19zrr1ki6sxm7msd60yv84cv1zd79ls4k45l"))))
      (inputs
       `(("sbcl-cffi" ,sbcl-cffi)
         ("bash" ,bash-minimal)
         ,@(package-inputs stumpwm-checkout)))
      (arguments
       (substitute-keyword-arguments
           (package-arguments stumpwm-checkout)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'substitute
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "primitives.lisp"
                   (("/bin/sh") (string-append (assoc-ref inputs "bash") "/bin/sh")))))
             (delete 'install-manual))))))))

(define-public sbcl-stumpwm-checkout-ttf-fonts
  (package
    (inherit sbcl-stumpwm-ttf-fonts)
    (inputs
     `(("stumpwm" ,stumpwm-checkout "lib")
       ("clx-truetype" ,sbcl-clx-truetype)))))

(define-stumpwm-next-package sbcl-stumpwm-next-ttf-fonts
  sbcl-stumpwm-checkout-ttf-fonts)

(define-public sbcl-stumpwm-checkout-globalwindows
  (package
    (inherit sbcl-stumpwm-globalwindows)
    (inputs
     `(("stumpwm" ,stumpwm-checkout "lib")))))

(define-stumpwm-next-package sbcl-stumpwm-next-globalwindows
  sbcl-stumpwm-checkout-globalwindows)

(define-public sbcl-stumpwm-checkout-swm-gaps
  (package
    (inherit sbcl-stumpwm-swm-gaps)
    (inputs
     `(("stumpwm" ,stumpwm-checkout "lib")))))

(define-stumpwm-next-package sbcl-stumpwm-next-swm-gaps
  sbcl-stumpwm-checkout-swm-gaps)

(define-public sbcl-clx-xembed
  (let ((commit "a5c4b844d31ee68ffa58c933cc1cdddde6990743")
        (revision "1"))
    (package
      (name "sbcl-xembed")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/laynor/clx-xembed.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1abx4v36ycmfjdwpjk4hh8058ya8whwia7ds9vd96q2qsrs57f12"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("clx" ,sbcl-clx)))
      (home-page "https://github.com/laynor/clx-xembed/")
      (synopsis "Implementation of the XEMBED protocol that integrates with CLX")
      (description "This package provides an implementation of the XEMBED
protocol that integrates with CLX.")
      (license license:expat))))

(define-public sbcl-stumpwm-checkout-stumptray
  (let ((commit "dd5b037923ec7d3cc27c55806bcec5a1b8cf4e91")
        (revision "1"))
    (package
      (name "sbcl-stumptray")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/stumpwm/stumpwm-contrib.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ahxdj9f884afpzxczx6mx7l4nwg4kw6afqaq7lwhf7lxcwylldn"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("stumpwm" ,stumpwm-checkout "lib")
         ("alexandria" ,sbcl-alexandria)
         ("xembed" ,sbcl-clx-xembed)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "modeline/stumptray"))))))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "")
      (description "")
      (license #f))))

(define-stumpwm-next-package sbcl-stumpwm-next-stumptray
  sbcl-stumpwm-checkout-stumptray)
