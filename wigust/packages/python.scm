;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
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

(define-public python-requests-2.18
  (package
    (inherit python-requests)
    (version "2.18.0")
    (name (string-append (package-name python-requests) "-" version))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests" version))
       (sha256
        (base32
         "0p7awxhzd81fwmjbsrx1bb0xkd4f8ynxmz8myyzq8wkqcbwqj0fd"))))))

(define-public python-certifi-2017.4.17
  (package
    (inherit python-certifi)
    (version "2017.4.17")
    (name (string-append (package-name python-certifi) "-" version))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "certifi" version))
       (sha256
        (base32
         "02n16i1dbp8cq974z0wh0pa11s0w2kfh77ksbzljqn31fjzpwlpp"))))))

(define-public python-urllib3-1.21.1
  (package
    (inherit python-urllib3)
    (version "1.21.1")
    (name (string-append (package-name python-urllib3) "-" version))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "urllib3" version))
       (sha256
        (base32
         "19c22gbkzs14dcaj4vvjgyfl0iqhhbbp7abblw0hkjhqhnbqci5i"))))))

(define-public python-idna-2.5
  (package
    (inherit python-idna)
    (version "2.5")
    (name (string-append (package-name python-idna) "-" version))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "idna" version))
       (sha256
        (base32
         "1ara12a7k2zc69msa0arrvw00gn61a6i6by01xb3lkkc0h4cxd9w"))))))

(define-public python-jwcrypto
  (package
    (name "python-jwcrypto")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jwcrypto" version))
       (sha256
        (base32
         "0rdnq9qhiazlxcn4birrakcx7fhzky29jvgp258adn895n9c0ym8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)))
    (home-page "https://github.com/latchset/jwcrypto")
    (synopsis "Implementation of JOSE Web standards")
    (description
     "This package provides an implementation of JOSE Web standards.")
    (license license:lgpl3+)))

(define-public python-github
  (package
    (name "python-github")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sigmavirus24/github3.py.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zmz488fk4swbmfsc61v8f8b01z3kar0w7wbq35m3m6r7wmgrjh3"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-ndg-httpsclient" ,python-ndg-httpsclient)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-betamax-matchers" ,python-betamax-matchers)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-uritemplate" ,python-uritemplate)
       ("python-dateutil" ,python-dateutil)
       ("python-requests" ,python-requests-2.18)
       ("python-certifi" ,python-certifi-2017.4.17)
       ("python-urllib3" ,python-urllib3-1.21.1)
       ("python-idna" ,python-idna-2.5)
       ("python-chardet" ,python-chardet)
       ("python-jwcrypto" ,python-jwcrypto)))
    (home-page "https://github3py.readthedocs.org")
    (synopsis "Python wrapper for the GitHub API")
    (description "Python wrapper for the GitHub API
@url{http://developer.github.com/v3}.")
    (license license:bsd-3)))

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
  (package
    (name "python-starred")
    (version "2.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maguowei/starred.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0acc4n73f2py709nh90yqylqd0v7v9ii1sgg49cazs2b73jzviad"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-github" ,python-github)
       ("python-urllib3" ,python-urllib3-1.21.1)))
    (home-page "https://github.com/maguowei/starred")
    (synopsis "Awesome List used GitHub stars")
    (description "Awesome List used GitHub stars.")
    (license license:expat)))

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

;; TODO: make test fails to found ddgr
(define-public python-ddgr
  (package
    (name "python-ddgr")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jarun/ddgr/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "0n1p8837qk86az0kazi7brphnffrg5kmp8blslywk7clcf48p0m9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python)))
    (arguments
     `(#:test-target
       "test"
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("\\./ddgr" line) (string-join (list "ls" "-l" line))))
             #t))
         (add-after 'unpack 'patch-shebangs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "ddgr"
               (("#!/usr/bin/env python3")
                (string-append "#!" (assoc-ref inputs "python") "/bin/python")))
             #t))
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check)))))
    (home-page "https://github.com/jarun/ddgr/")
    (synopsis "DuckDuckGo from the terminal")
    (description "@code{ddgr} provides a command-line interface to
@url{https://duckduckgo.com/,DuckDuckGo} search engine.

Features:

@itemize
@item Fast and clean (no ads, stray URLs or clutter), custom color
@item Navigate result pages from omniprompt, open URLs in browser
@item Search and option completion scripts for Bash, Zsh and Fish
@item DuckDuckGo Bang support (along with completion)
@item Open the first result directly in browser (as in I'm Feeling Ducky)
@item Non-stop searches: fire new searches at omniprompt without exiting
@item Keywords (e.g. filetype:mime, site:somesite.com) support
@item Specify region, disable safe search
@item HTTPS proxy support, Do Not Track set, optionally disable User Agent
@item Support custom url handler script or cmdline utility
@item Comprehensive documentation, man page with handy usage examples
@item Minimal dependencies
@end itemize\n")
    (license license:gpl3+)))

(define-public python-unicode
  (package
    (name "python-unicode")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://kassiopeia.juls.savba.sk/~garabik/software/unicode/"
             "unicode_" version ".tar.gz"))
       (sha256
        (base32
         "0p01axlakaksjmrfndh9ly313rc306yq3qjisda2h9fc54ih8wwg"))))
    (build-system python-build-system)
    (home-page "http://kassiopeia.juls.savba.sk/")
    (synopsis "Display unicode character properties")
    (description "@code{unicode} is a command-line utility that
displays properties for a given unicode character, or searches unicode
database for a given name.")
    (license license:gpl3+)))

(define-public python2-unicode
  (package-with-python2 python-unicode))
