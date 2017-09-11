;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-ranger-fm
  (package
    (name "python-ranger-fm")
    (version "1.9.0b5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ranger-fm" version))
       (sha256
        (base32
         "0iqghgyhrn27pfyl63mn6g8lkd3768mqk6gn8gfg2hysfd1p8gfw"))))
    (build-system python-build-system)
    (home-page "http://ranger.nongnu.org")
    (synopsis "Vim-like file manager")
    (description "Vim-like file manager")
    (license #f)))

(define-public python-github
  (let ((commit "c82e90e5bd65eaadeabbdbbbe37bc7e3dc295b3e")
        (revision "1"))
    (package
      (name "python-github")
      (version (string-append "1.0.0a4" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sigmavirus24/github3.py.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "17vfzjshr5p567x28zvilcsy0n814hcx4wjiz185n7p44sgc577r"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-ndg-httpsclient" ,python-ndg-httpsclient)
         ("python-pyasn1" ,python-pyasn1)
         ("python-pyopenssl" ,python-pyopenssl)
         ("python-betamax-matchers" ,python-betamax-matchers)
         ("python-mock" ,python-mock)
         ("python-pytest" ,python-pytest)
         ("python-uritemplate" ,python-uritemplate)))
      (home-page "https://github3py.readthedocs.org")
      (synopsis "Python wrapper for the GitHub API")
      (description "Python wrapper for the GitHub API
@url{http://developer.github.com/v3}.")
      (license license:bsd-3))))

(define-public python2-github
  (let ((base (package-with-python2
               (strip-python2-variant python-github))))
    (package
      (inherit base)
      (propagated-inputs
       `(("python2-unittest2" ,python2-unittest2)
         ,@(package-propagated-inputs base))))))

(define-public python-livereload
  (package
    (name "python-livereload")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "livereload" version))
       (sha256
        (base32
         "0b2yyfnpddmrwjfqsndidzchkf3l9jlgzfkwl8dplim9gq6y2ba2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-tornado" ,python-tornado)))
    (home-page "https://github.com/lepture/python-livereload")
    (synopsis "Python LiveReload is an awesome tool for web developers")
    (description "Python LiveReload is an awesome tool for web developers")
    (license license:bsd-3)))

(define-public python-starred
  (let ((commit "aa4c010c791a4f84f6e26f96685552ef2ef3a4e8")
        (revision "1"))
    (package
      (name "python-starred")
      (version (string-append "1.3.1" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/maguowei/starred.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "181jr5r4m1dkb081jynlkdmy8cd55673vl8lwzb34q6diqds844a"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-click" ,python-click)
         ("python-github" ,python-github)))
      (home-page "https://github.com/maguowei/starred")
      (synopsis "Awesome List used GitHub stars")
      (description "Awesome List used GitHub stars.")
      (license license:expat))))

(define-public python2-starred
  (let ((base (package-with-python2
               (strip-python2-variant python-starred))))
    (package
      (inherit base)
      (propagated-inputs
       `(("python2-github2", python2-github)
         ,@(alist-delete "python-github"
                         (package-propagated-inputs base)
                         equal?))))))
