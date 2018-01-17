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
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
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

(define-public python-iso-639
  (package
    (name "python-iso-639")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iso-639" version))
       (sha256
        (base32
         "0jffmh4m20q8j27xb2fqbnlghjj0cx8pgsbzqisdg65qh2wd976w"))))
    (build-system python-build-system)
    (home-page "https://github.com/noumar/iso639")
    (synopsis "Python library for ISO 639 standard")
    (description "This package provides a Python library for ISO 639
standard.")
    (license license:agpl3+)))

(define-public python2-iso-639
  (package-with-python2 python-iso-639))

(define-public python-iso3166
  (package
    (name "python-iso3166")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iso3166" version))
       (sha256
        (base32
         "0cs9w507dj93jj9z9di93lx2fplf8pma4jkrsvvb258dv6z1gszv"))))
    (build-system python-build-system)
    (home-page
     "http://github.com/deactivated/python-iso3166")
    (synopsis "Self-contained ISO 3166-1 country definitions")
    (description "This package provides a ISO 3166-1 country definitions.")
    (license license:expat)))

(define-public python2-iso3166
  (package-with-python2 python-iso3166))

(define-public python-pycryptodome
  (package
    ;; FIXME: Skipping GMP tests (requires 'mpir' library)
    (name "python-pycryptodome")
    (version "3.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodome" version))
       (sha256
        (base32
         "1xrsd6ql4w0ypkxnsg3fivs3r3z6dd93x44lhvam7jzh3gixzn0q"))))
    (build-system python-build-system)
    (home-page "http://www.pycryptodome.org")
    (synopsis "Cryptographic library for Python")
    (description "This package provides a cryptographic library for Python.")
    (license license:bsd-2)))

(define-public python2-pycryptodome
  (package-with-python2 python-pycryptodome))

(define-public streamlink
  (package
    (name "streamlink")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "streamlink" version))
       (sha256
        (base32
         "1in0jqg9xpqwjkzskyy2iyq8zcfmsqich18mfyr47g62y3pjy98a"))))
    (build-system python-build-system)
    (home-page "https://github.com/streamlink/streamlink")
    (inputs
     `(("python-pysocks" ,python-pysocks)
       ("python-websocket-client" ,python-websocket-client)
       ("python-iso3166" ,python-iso3166)
       ("python-iso-639" ,python-iso-639)
       ("python-pycryptodome" ,python-pycryptodome)
       ("python-requests" ,python-requests)
       ("python-pytest" ,python-pytest)
       ("python-mock" ,python-mock)))
    (synopsis "Extract streams from various services")
    (description "Streamlink is command-line utility that extracts streams
from various services and pipes them into a video player of choice.")
    (license license:bsd-2)))
