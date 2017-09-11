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

(define-module (wigust packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby))

(define-public ruby-guard-livereload
  (package
    (name "ruby-guard-livereload")
    (version "2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "guard-livereload" version))
       (sha256
        (base32
         "0yd74gdbbv2yz2caqwpsavzw8d5fd5y446wp8rdjw8wan0yd6k8j"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-em-websocket" ,ruby-em-websocket)
       ("ruby-guard" ,ruby-guard)
       ("ruby-guard-compat" ,ruby-guard-compat)
       ("ruby-multi-json" ,ruby-multi-json)))
    (arguments
     '(#:tests? #f)) ; no required file
    (synopsis "Automatically reloads your browser when files are modified.")
    (description "LiveReload automatically reloads your browser when 'view'
files are modified.")
    (home-page "https://rubygems.org/gems/guard-livereload")
    (license license:expat)))

(define-public ruby-http-parser-rb
  (package
    (name "ruby-http-parser.rb")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "http_parser.rb" version))
       (sha256
        (base32
         "15nidriy0v5yqfjsgsra51wmknxci2n2grliz78sf9pga3n0l7gi"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no required file
    (synopsis "Ruby bindings")
    (description "Ruby bindings.")
    (home-page "http://github.com/tmm1/http_parser.rb")
    (license license:expat)))

(define-public ruby-em-websocket
  (package
    (name "ruby-em-websocket")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "em-websocket" version))
       (sha256
        (base32
         "1bsw8vjz0z267j40nhbmrvfz7dvacq4p0pagvyp17jif6mj6v7n3"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-eventmachine" ,ruby-eventmachine)
       ("ruby-http-parser-rb" ,ruby-http-parser-rb)))
    (arguments
     '(#:tests? #f)) ; no required file
    (synopsis "EventMachine based WebSocket server")
    (description "EventMachine based WebSocket server")
    (home-page "http://github.com/igrigorik/em-websocket")
    (license #f)))


(define-public ruby-gitlab
  (package
    (name "ruby-gitlab")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "gitlab" version))
       (sha256
        (base32
         "0wzazdmzhw72r1kkj8ckb1l9ryas582r5d7c6qb609kkfs31rg6m"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-httparty" ,ruby-httparty)
       ("ruby-terminal-table" ,ruby-terminal-table)))
    (arguments
     `(#:tests? #f)) ; depends on non-existing file
    (synopsis "Ruby client and CLI for GitLab API")
    (description "Ruby client and CLI for GitLab API.")
    (home-page "https://github.com/narkoz/gitlab")
    (license license:bsd-3)))

(define-public ruby-guard-compat
  (package
    (name "ruby-guard-compat")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "guard-compat" version))
       (sha256
        (base32
         "1zj6sr1k8w59mmi27rsii0v8xyy2rnsi09nqvwpgj1q10yq1mlis"))))
    (arguments
     '(#:tests? #f)) ; no required file
    (build-system ruby-build-system)
    (synopsis "Helps creating valid Guard plugins and testing them")
    (description "Helps creating valid Guard plugins and testing them.")
    (home-page "")
    (license license:expat)))


(define-public ruby-httparty
  (package
    (name "ruby-httparty")
    (version "0.15.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "httparty" version))
       (sha256
        (base32
         "0akybx7jkzhhf7f9na12jssfcllma905c8nl93ia89akbbicvq7v"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-multi-xml" ,ruby-multi-xml)))
    (arguments
     `(#:tests? #f)) ; depends on non-existing file
    (synopsis "Makes http fun")
    (description "Makes http fun! Also, makes consuming restful web services
dead easy.")
    (home-page "http://jnunemaker.github.com/httparty")
    (license license:expat)))

(define-public ruby-multi-json
  (package
    (name "ruby-multi-json")
    (version "1.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_json" version))
       (sha256
        (base32
         "1raim9ddjh672m32psaa9niw67ywzjbxbdb8iijx3wv9k5b0pk2x"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (synopsis "A common interface to multiple JSON libraries")
    (description "This package provides a common interface to multiple JSON
libraries, including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby
JSON gem, NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (home-page "http://github.com/intridea/multi_json")
    (license license:expat)))

(define-public ruby-multi-xml
  (package
    (name "ruby-multi-xml")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_xml" version))
       (sha256
        (base32
         "0lmd4f401mvravi1i1yq7b2qjjli0yq7dfc4p1nj5nwajp7r6hyj"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "Provides swappable XML backends utilizing LibXML")
    (description "Provides swappable XML backends utilizing LibXML, Nokogiri,
Ox, or REXML.")
    (home-page "https://github.com/sferik/multi_xml")
    (license license:expat)))


(define-public ruby-terminal-table
  (package
    (name "ruby-terminal-table")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "terminal-table" version))
       (sha256
        (base32
         "1512cngw35hsmhvw4c05rscihc59mnj09m249sm9p3pik831ydqk"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-unicode-display-width" ,ruby-unicode-display-width)))
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "Simple, feature rich ascii table generation library")
    (description "Simple, feature rich ascii table generation library.")
    (home-page "https://github.com/tj/terminal-table")
    (license license:expat)))


(define-public ruby-unicode-display-width
  (package
    (name "ruby-unicode-display-width")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "unicode-display_width" version))
       (sha256
        (base32
         "12pi0gwqdnbx1lv5136v3vyr0img9wr0kxcn4wn54ipq4y41zxq8"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-unicode-emoji" ,ruby-unicode-emoji)))
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "Determines the monospace display width")
    (description "Determines the monospace display width of a string using
EastAsianWidth.txt, Unicode general category, and other data.")
    (home-page "http://github.com/janlelis/unicode-display_width")
    (license license:expat)))


(define-public ruby-unicode-emoji
  (package
    (name "ruby-unicode-emoji")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "unicode-emoji" version))
       (sha256
        (base32
         "1hrsvkdpsi534fqlk2wxvdvgykk89sajhsx7jskjncpqf8cxfcgr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "Retrieve emoji data about Unicode codepoints")
    (description "Retrieve emoji data about Unicode codepoints.  Also contains
a regex to match emoji.")
    (home-page "https://github.com/janlelis/unicode-emoji")
    (license license:expat)))

