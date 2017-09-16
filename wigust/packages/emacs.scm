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

(define-module (wigust packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages chez)
  #:use-module (gnu packages code)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages image)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages emacs)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public emacs-company-lua
  (let ((commit "0be8122f3adf57ad27953bf4b03545d6298d3da4")
        (revision "1"))
    (package
      (name "emacs-company-lua")
      (version "0.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ptrv/company-lua.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1d9i165apgmwns7b2fd5wcpjpkah3dyj20v5sb8ynvz6qhhr5r9c"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-company" ,emacs-company)
         ("emacs-s" ,emacs-s)
         ("emacs-f" ,emacs-f)
         ("emacs-lua-mode" ,emacs-lua-mode)))
      (home-page "https://github.com/ptrv/company-lua")
      (synopsis "Company backend for Lua")
      (description "Company backend for Lua")
      (license license:gpl3+))))

(define-public emacs-company-quickhelp
  (package
    (name "emacs-company-quickhelp")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/expez/company-quickhelp/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xrn2z1dgk5gmkmp2jkn9g83ckk39lqp5pyyv8rl7f6gqvib3qh0"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-pos-tip" ,emacs-pos-tip)
       ("emacs-company" ,emacs-company)))
    (home-page "https://www.github.com/expez/company-quickhelp")
    (synopsis "Popup documentation for completion candidates")
    (description "Emacs @code{company-mode} extension to show a documentation
for the completion candidate.")
    (license license:gpl3+)))

(define-public emacs-company-tern
  (package
    (name "emacs-company-tern")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/proofit404/company-tern/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "1qgrgnbajgnsnx4az4ajnlrhc73q0xxjikk617nf3cs87x4772a6"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-company" ,emacs-company)
       ("emacs-tern" ,emacs-tern)
       ("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)))
    (home-page "https://github.com/proofit404/company-tern")
    (synopsis "Tern backend for company-mode")
    (description "Tern backend for company-mode.")
    (license license:gpl3+)))

(define-public emacs-discover-my-major
  (package
    (name "emacs-discover-my-major")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/steckerhalter/discover-my-major"
                       "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nah41f92rrl2l405kpqr6iaks11jyclgl4z7ilfymbr4ifmsiyl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-makey" ,emacs-makey)))
    (home-page
     "https://github.com/steckerhalter/discover-my-major")
    (synopsis "Discover key bindings for the current Emacs major mode")
    (description
     "Discover key bindings and their meaning for the current Emacs major mode ")
    (license license:gpl3+)))

(define-public emacs-dumb-jump
  (package
    (name "emacs-dumb-jump")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jacktasia/dumb-jump/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "14sfnlgfc81y2il5v0njvmdh39lxpy8kz4j0prlc2rzd2bk0a8n5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-f" ,emacs-f)
       ("emacs-s" ,emacs-s)
       ("emacs-dash" ,emacs-dash)
       ("emacs-popup" ,emacs-popup)))
    (home-page "https://github.com/jacktasia/dumb-jump")
    (synopsis "Jump to definition for multiple languages without configuration")
    (description "Dumb Jump is an Emacs \"jump to definition\" package with
support for multiple programming languages that favors \"just working\" over
speed or accuracy.  This means minimal -- and ideally zero -- configuration
with absolutely no stored indexes (TAGS) or persistent background processes.
Dumb Jump performs best with The Silver Searcher `ag` or ripgrep `rg`
installed.  Dumb Jump requires at least GNU Emacs 24.3. ")
    (license license:gpl3+)))

(define-public emacs-edit-indirect
  (package
    (name "emacs-edit-indirect")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Fanael/edit-indirect/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "07kr58rd1p5j764wminsssazr73hy51yw8iqcsv5z2dwgj7msv71"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/edit-indirect")
    (synopsis "Edit regions in separate buffers")
    (description "Edit regions in separate buffers, like
@code{org-edit-src-code} but for arbitrary regions.")
  (license license:gpl3+)))

(define-public emacs-edit-server
  (package
    (name "emacs-edit-server")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/stsquad/emacs_chrome/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1r92kqggslqasza718z4ka883mqfbnibdm43f0j9gaipk0msm2wf"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-elisp
           ;; Elisp directory is not in root of the source.
           (lambda _
             (chdir "servers"))))))
    (home-page "https://github.com/stsquad/emacs_chrome")
    (synopsis "Server that responds to edit requests from Chromium")
    (description "Provides an edit server to respond to requests from Emacs.

To open pages for editing in new buffers in your existing Emacs
instance:

@code{(when (require 'edit-server nil t)
        (setq edit-server-new-frame nil)
        (edit-server-start))}

To open pages for editing in new frames using a running emacs
started in --daemon mode:

@code{(when (and (require 'edit-server nil t) (daemonp))
        (edit-server-start))}

Buffers are edited in `text-mode' by default; to use a different
major mode, change `edit-server-default-major-mode' or customize
`edit-server-url-major-mode-alist' to specify major modes based
on the remote URL:

@code{(setq edit-server-url-major-mode-alist
        '((\"github\\\\.com\" . markdown-mode)))}

Alternatively, set the mode in `edit-server-start-hook'.  For
example:

@code{(add-hook 'edit-server-start-hook
         (lambda ()
           (when (string-match \"github.com\" (buffer-name))
             (markdown-mode))))}")
    (license license:gpl3+)))

(define-public emacs-elisp-refs
  (package
    (name "emacs-elisp-refs")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Wilfred/elisp-refs/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fj6wphwrvbslw46w7wgdk3b4bfr312ygj3lbgr9qw63lpqw26nl"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)
       ("emacs-list-utils" ,emacs-list-utils)
       ("emacs-loop" ,emacs-loop)
       ("emacs-s" ,emacs-s)))
    (home-page "https://github.com/Wilfred/elisp-refs")
    (synopsis "Find callers of elisp functions or macros")
    (description "Find references to functions, macros or variables.  Unlike a
dumb text search, @code{elisp-refs} actually parses the code, so it's never
confused by comments or @code{foo-bar} matching @code{foo}.")
    (license license:gpl3+)))

(define-public emacs-ggtags
  (package
    (name "emacs-ggtags")
    (version "0.8.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/ggtags-"
                           version ".el"))
       (sha256
        (base32
         "0ny3llk021g6r0s75xdm4hzpbxv393ddm2r6f2xdk8kqnq4gnirp"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/leoliu/ggtags")
    (synopsis "Frontend to GNU Global source code tagging system")
    (description "Emacs frontend to GNU Global source code tagging system.

The goal is to make working with GNU Global in Emacs as effortlessly and
intuitively as possible and to integrate tightly with standard emacs packages.

Features:
@itemize
@item Build on @code{compile.el} for asynchronicity and its large feature-set.
@item Automatically update Global's tag files when needed with tuning for
large source trees.
@item Intuitive navigation among multiple matches with mode-line display of
current match, total matches and exit status.
@item Read tag with completion.
@item Show definition at point.
@item Jump to #include files.
@item Support search history and saving a search to register/bookmark.
@item Query replace.
@item Manage Global's environment variables on a per-project basis.
@item Highlight (definition) tag at point.
@item Abbreviated display of file names.
@item Support all Global search backends: @code{grep}, @code{idutils}, etc.
@item Support exuberant ctags @url{http://ctags.sourceforge.net/} and
@code{pygments} backend.
@item Support all Global's output formats: @code{grep}, @code{ctags-x},
@code{cscope} etc.
@item Support projects on remote hosts (e.g. via ``tramp``).
@item Support eldoc.
@item Search @code{GTAGSLIBPATH} for references and symbols.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-helm-youtube
  (let ((revision "1")
        (commit "202c27fc3b54927611e9d9c764465e1b42ef7e41"))
    (package
      (name "emacs-helm-youtube")
      (version "1.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/maximus12793/helm-youtube.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1wqxcz03fq2z31a1n90dg9ap3451vx1376ijbpfy9sg66pgj8yxv"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-request" ,emacs-request)
         ("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/maximus12793/helm-youtube")
      (synopsis "Query YouTube and play videos in your browser")
      (description "This package provides an interactive prompt to search on
Youtube.")
      (license license:gpl3+))))

(define-public emacs-helpful
  (package
    (name "emacs-helpful")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Wilfred/helpful/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16dx566qzrjj0bf43lnw7h1qlvgs94brqplamw8kppp2ylr72qs9"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-elisp-refs" ,emacs-elisp-refs)))
    (home-page "https://github.com/Wilfred/helpful")
    (synopsis "More contextual information in Emacs help")
    (description "@code{helpful} is an alternative to the built-in Emacs help
that provides much more contextual information.

@itemize
@item Show the source code for interactively defined functions (unlike the
built-in Help).
@item Fall back to the raw sexp if no source is available.
@item Show where a function is being called.
@item Docstrings will Highlight the summary (the first sentence), include
cross-references, hide superfluous puncuation.
@item Show you the properties that have been applied to the current
symbol.  This provides visibility of features like edebug or byte-code
optimisation.
@item Provide a separate @code{helpful-command} function to view interactive
functions.
@item Display any keybindings that apply to interactive functions.
@item Trace, disassemble functions from inside Helpful.  This is discoverable
and doesn't require memorisation of commands.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-highlight-defined
  (package
    (name "emacs-highlight-defined")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Fanael/highlight-defined/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ryd66989b5byqdw8jmjrjf0c78iiz72wibld750skcnj5h5h506"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/highlight-defined")
    (synopsis "Syntax highlighting of known Elisp symbols")
    (description "Minor mode providing syntax highlighting of known Emacs Lisp
symbols.  Currently the code distinguishes Lisp functions, built-in functions,
macros, faces and variables.  To enable call @code{highlight-defined-mode}. ")
    (license license:gpl3+)))

(define-public emacs-highlight-numbers
  (package
    (name "emacs-highlight-numbers")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Fanael/highlight-numbers/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "030v5p11d4n0581ncv499l1fqrmfziy756q6378x2bv22ixghqqp"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-parent-mode" ,emacs-parent-mode)))
    (home-page "https://github.com/Fanael/highlight-numbers")
    (synopsis "Highlight numbers in source code")
    (description "@code{highlight-numbers-mode} provides a minor mode for
syntax highlighting of numeric literals in source code.

It s customizable: it's easy to add or redefine what exactly consitutes a
\"number\" in given major mode.  See @code{highlight-numbers-modelist}.")
    (license license:gpl3+)))

(define-public emacs-indium
  (package
    (name "emacs-indium")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/NicolasPetton/Indium/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "1r4cag75w11giihh5kkczppqibwc0qr237s5p00y5cvv6z3hhy8g"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-seq" ,emacs-seq)
       ("emacs-js2-mode" ,emacs-js2-mode)
       ("emacs-company" ,emacs-company)
       ("emacs-websocket" ,emacs-websocket)
       ("emacs-memoize" ,emacs-memoize)
       ("emacs-sourcemap" ,emacs-sourcemap)))
    (home-page "https://github.com/NicolasPetton/indium")
    (synopsis "JavaScript Awesome Development Environment")
    (description "Indium connects to a browser tab or nodejs process and
provides many features for JavaScript development, including a REPL (with auto
completion) & object inspection, an inspector, with history and navigation,
and even a stepping Debugger, similar to @code{edebug}, or @code{cider}.")
    (license license:gpl3+)))

(define-public emacs-json-mode
  (package
    (name "emacs-json-mode")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/joshwnj/json-mode/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "06h45p4cn767pk9sqi2zb1c65wy5gyyijqxzpglp80zwxhvajdz5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-json-reformat" ,emacs-json-reformat)
       ("emacs-json-snatcher" ,emacs-json-snatcher)))
    (home-page
     "https://github.com/joshwnj/json-mode")
    (synopsis "Major mode for editing JSON files.")
    (description "Extend the builtin js-mode's syntax highlighting ")
    (license license:gpl3+)))

(define-public emacs-json-reformat
  (package
    (name "emacs-json-reformat")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/gongo/json-reformat/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "11fbq4scrgr7m0iwnzcrn2g7xvqwm2gf82sa7zy1l0nil7265p28"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/gongo/json-reformat")
    (synopsis "Reformatting tool for JSON")
    (description "json-reformat.el is a reformatting tool for
JSON (http://json.org/).

## Usage

  1. Specify region
  2. Call 'M-x json-reformat-region'

## Customize

  - `json-reformat:indent-width'
  - `json-reformat:pretty-string?'
")
    (license license:gpl3+)))

(define-public emacs-json-snatcher
  (package
    (name "emacs-json-snatcher")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Sterlingg/json-snatcher/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "1nfiwsifpdiz0lbrqa77nl0crnfrv5h85ans9b0g5rggnmyshcfb"))))
    (build-system emacs-build-system)
    (home-page "http://github.com/sterlingg/json-snatcher")
    (synopsis "Grabs the path to JSON values in a JSON file")
    (description "Well this was my first excursion into ELisp programmming.
It didn't go too badly once I fiddled around with a bunch of the functions.

The process of getting the path to a JSON value at point starts with
a call to the jsons-print-path function.

It works by parsing the current buffer into a list of parse tree nodes
if the buffer hasn't already been parsed in the current Emacs session.
While parsing, the region occupied by the node is recorded into the
jsons-parsed-regions hash table as a list.The list contains the location
of the first character occupied by the node, the location of the last
character occupied, and the path to the node.  The parse tree is also stored
in the jsons-parsed list for possible future use.

Once the buffer has been parsed, the node at point is looked up in the
jsons-curr-region list, which is the list of regions described in the
previous paragraph for the current buffer.  If point is not in one of these
interval ranges nil is returned, otherwise the path to the value is returned
in the form [<key-string>] for objects, and [<loc-int>] for arrays.
eg: ['value1'][0]['value2'] gets the array at with name value1, then gets the
0th element of the array (another object), then gets the value at 'value2'.


Installation:

IMPORTANT: Works ONLY in Emacs 24 due to the use of the lexical-binding variable.

To install add the json-snatcher.el file to your load-path, and
add the following lines to your .emacs file:
(require 'json-snatcher)
(defun js-mode-bindings ()
  \"Sets a hotkey for using the json-snatcher plugin.\"
  (when (string-match  \"\\\\.json$\" (buffer-name))
      (local-set-key (kbd \"C-c C-g\") 'jsons-print-path)))
(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)

This binds the key to snatch the path to the JSON value to C-c C-g only
when either JS mode, or JS2 mode is active on a buffer ending with
the .json extension.")
    (license license:gpl3+)))

(define-public emacs-list-utils
  (package
    (name "emacs-list-utils")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rolandwalker/list-utils/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xc1xh8c82h5gdjbgpdsdclgwxkxbb7h3x3a2bscpm41g8pnan4p"))))
    (build-system emacs-build-system)
    (home-page "http://github.com/rolandwalker/list-utils")
    (synopsis "List-manipulation utility functions")
    (description "List manipulation library for Emacs.")
    (license license:gpl3+)))

(define-public emacs-loop
  (package
    (name "emacs-loop")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Wilfred/loop.el/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "1z3rhh3zyjabz36410yz0lp4a0qwwj0387as662wvx3z9y54jia9"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Wilfred/loop.el")
    (synopsis "Imperative loop structures for Emacs")
    (description "Loop structures familiar to users of other languages.  This
library adds a selection of popular loop structures as well as break and
continue.")
    (license license:gpl3+)))

(define-public emacs-makey
  (package
    (name "emacs-makey")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mickeynp/makey/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kzl4q1wf2zhkx9nrymxa67n99iq0bj7zqhpaz4byksna1hsxfmv"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mickeynp/makey")
    (synopsis "Interactive commandline mode")
    (description "Interactive commandline mode")
    (license license:gpl3+)))

(define-public emacs-nnreddit
  (let ((commit "9843f99d01fd8f1eea2fc685965a7c7f4eeb187a")
        (revision "1"))
    (package
      (name "emacs-nnreddit")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/paul-issartel/nnreddit.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0j4h3bnga640250jdq8bwyja49r41ssrsjd6lba4gzzllqk02nbn"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/paul-issartel/nnreddit")
      (synopsis "Reddit backend for the Gnus newsreader")
      (description "@url{https://www.reddit.com} backend for the Gnus
newsreader.")
      (license license:gpl3+))))

(define-public emacs-npm-mode
  (package
    (name "emacs-npm-mode")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mojochao/npm-mode/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1kq1ww22dwf8c2i2b4z2ldbbmnihj65kb7n5vzvwkch9h4hxpqh5"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mojochao/npm-mode")
    (synopsis "Minor mode for working with npm projects")
    (description "@code{npm-mode} provides a minor mode to work with npm
projects.")
    (license license:gpl3+)))
(define-public emacs-org-protocol-capture-html
  (let ((commit "0e39b7e2261599d28e6bbd094a0657d9315719bc")
        (revision "1"))
    (package
      (name "emacs-org-protocol-capture-html")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/alphapapa/org-protocol-capture-html.git")
           (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0l80nb2dxfm1bws1rqqkavswnpyqbwlv84q1zp4lrsfarjb3l56c"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("pandoc" ,ghc-pandoc)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-shell-script
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "org-protocol-capture-html.sh"
                             (string-append (assoc-ref outputs "out")
                                            "/bin")))))))
      (home-page "https://github.com/alphapapa/org-protocol-capture-html.git")
      (synopsis "Captures Web pages into Org using Pandoc to process HTML")
      (description "This package captures Web pages into Org-mode using Pandoc to
process HTML.  It can also use eww's eww-readable functionality to
get the main content of a page.

These are the helper functions that run in Emacs.  To capture pages
into Emacs, you can use either a browser bookmarklet or the
org-protocol-capture-html.sh shell script.  See the README.org file
for instructions.")
      (license license:gpl3+))))

(define-public emacs-parent-mode
  (package
    (name "emacs-parent-mode")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Fanael/parent-mode/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0gxbl5s1w96v6v55b7aaansgw4sxhzfx9nrsvpk3pfhsibs6yqjd"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/parent-mode")
    (synopsis "Get major mode's parent modes")
    (description "Get major mode's parent modes")
    (license license:gpl3+)))

(define-public emacs-pos-tip
  (package
    (name "emacs-pos-tip")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.emacswiki.org/emacs/download/pos-tip.el")
       (file-name (string-append "pos-tip-" version ".el"))
       (sha256
        (base32
         "1c14693h903mbgapks9zgxl6l3pkipc5r7n4ik0szjl4hsghc4z3"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/pos-tip.el")
    (synopsis "Function to display a tooltip at mouse position")
    (description "The standard library tooltip.el provides the function for
displaying a tooltip at mouse position which allows users to easily show it.
However, locating tooltip at arbitrary buffer position in window is not easy.
This program provides such function to be used by other frontend programs.")
    (license license:gpl2+)))

(define-public emacs-rjsx-mode
  (package
    (name "emacs-rjsx-mode")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/felipeochoa/rjsx-mode/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vlk298g79nz0cw5sgnp2683gr72x5sfsqbyxjsg521ka3lyjq98"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-js2-mode" ,emacs-js2-mode)))
    (home-page "https://github.com/felipeochoa/rjsx-mode/")
    (synopsis "Real support for JSX")
    (description "Defines a major mode @code{rjsx-mode} based on
@code{js2-mode} for editing JSX files.  @code{rjsx-mode} extends the parser in
@code{js2-mode} to support the full JSX syntax.  This means you get all of the
@code{js2} features plus proper syntax checking and highlighting of JSX code
blocks.

Some features that this mode adds to js2:

@itemize
@item Highlighting JSX tag names and attributes (using the rjsx-tag and
rjsx-attr faces)
@item Highlight undeclared JSX components
@item Parsing the spread operator {...otherProps}
@item Parsing && and || in child expressions {cond && <BigComponent/>}
@item Parsing ternary expressions {toggle ? <ToggleOn /> : <ToggleOff />}
@end itemize

Additionally, since rjsx-mode extends the js2 AST, utilities using
the parse tree gain access to the JSX structure.")
    (license license:gpl3+)))

(define-public emacs-seq
  (package
    (name "emacs-seq")
    (version "2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://elpa.gnu.org/packages/seq-" version ".tar"))
       (sha256
        (base32
         "0vrpx6nnyjb0gsypknzagimlhvcvi5y1rcdkpxyqr42415zr8d0n"))))
    (build-system emacs-build-system)
    (home-page "http://elpa.gnu.org/packages/seq.html")
    (synopsis "Sequence manipulation functions")
    (description "Sequence-manipulation functions that complement basic
functions provided by subr.el.

All functions are prefixed with \"seq-\".

All provided functions work on lists, strings and vectors.

Functions taking a predicate or iterating over a sequence using a function as
argument take the function as their first argument and the sequence as their
second argument.  All other functions take the sequence as their first
argument.")
    (license license:gpl3+)))

(define-public emacs-skewer-mode
  (package
    (name "emacs-skewer-mode")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/skeeto/skewer-mode/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07jpz374j0j964szy3zznrkyja2kpdl3xa87wh7349mzxivqxdx0"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-simple-httpd" ,emacs-simple-httpd)
       ("emacs-js2-mode" ,emacs-js2-mode)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-html-js
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((skewer-mode (string-append (assoc-ref outputs "out")
                                               "/share/emacs/site-lisp/guix.d"
                                               "/skewer-mode-"
                                               ,version)))
               (for-each (lambda (file)
                           (install-file file skewer-mode))
                         '("example.html"
                           "skewer-everything.user.js"
                           "skewer.js")))
             #t)))))
    (home-page "https://github.com/skeeto/skewer-mode")
    (synopsis "Live web development in Emacs")
    (description
     "Skewer-mode provides live interaction with JavaScript, CSS, and HTML in
a web browser.  Expressions are sent on-the-fly from an editing buffer to be
evaluated in the browser, just like Emacs does with an inferior Lisp process
in Lisp modes.")
    (license license:unlicense)))

(define-public emacs-slime-company
  (package
    (name "emacs-slime-company")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/anwyn/slime-company/archive/"
                           "v" version ".tar.gz"))
       (sha256
        (base32
         "1myl79pxj501xfr5qc5a24qddsn2l5iaamg7rf7fpny7mr9v70ar"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-slime" ,emacs-slime)
       ("emacs-company" ,emacs-company)))
    (home-page "http://company-mode.github.io")
    (synopsis
     "slime completion backend for company mode")
    (description
     "This is a backend implementation for the completion package company-mode
by Nikolaj Schumacher. Supports the normal and the fuzzy completion modes of
SLIME.

Installation:

 Put this file somewhere into your load-path
 (or just into slime-path/contribs) and then call

  (slime-setup '(slime-company))

Also have the following, more convenient key bindings for company mode in my
.emacs:

  (define-key company-active-map (kbd \"\\C-n\") 'company-select-next)
  (define-key company-active-map (kbd \"\\C-p\") 'company-select-previous)
  (define-key company-active-map (kbd \"\\C-d\") 'company-show-doc-buffer)
  (define-key company-active-map (kbd \"M-.\") 'company-show-location)
")
    (license license:gpl3+)))

(define-public emacs-sourcemap
  (package
    (name "emacs-sourcemap")
    (version "0.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/syohex/emacs-sourcemap/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bmd5l3cx2iyl7vxn84xdhs80b07kpdpfwki28lh5d0kmm5qs6m6"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/syohex/emacs-sourcemap")
    (synopsis "Sourcemap parser")
    (description "Sourcemap parser")
    (license license:gpl3+)))

(define-public emacs-sr-speedbar
  (package
    (name "emacs-sr-speedbar")
    (version "20161025")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.emacswiki.org/emacs/download/sr-speedbar.el")
       (file-name (string-append "sr-speedbar-" version ".el"))
       (sha256
        (base32
         "15kvl270a5xx1w5fjlrawslnpwyks2x17356xcr0idhv5xw2wn30"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/download/sr-speedbar.el")
    (synopsis "Same frame Emacs @code{speedbar}")
    (description "Show the speedbar in the same Emacs frame or in an extra
window.  Customize the inital width of the speedbar.")
    (license license:gpl3+)))

(define-public emacs-stickyfunc-enhance
  (let ((commit "13bdba51fcd83ccbc3267959d23afc94d458dcb0")
        (revision "1"))
    (package
      (name "emacs-stickyfunc-enhance")
      (version "0.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tuhdo/semantic-stickyfunc-enhance.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "16dxjsr5nj20blww4xpd4jzgjprzzh1nwvb810ggdmp9paf4iy0g"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/tuhdo/semantic-stickyfunc-enhance")
      (synopsis "Enhancement to stock @code{semantic-stickyfunc-mode}")
      (description
       "@code{semantic-stickyfunc-mode} shows the function point is currently
in at the first line of the current buffer.  This is useful when you have a
very long function that spreads more than a screen, and you don't have to
scroll up to read the function name and then scroll down to original position.")
      (license license:gpl3+))))

(define-public emacs-stumpwm-mode
  (let ((commit "8fbe071d2c6c040794060a354eb377218dc10b35")
        (revision "1"))
    (package
      (name "emacs-stumpwm-mode")
      (version (string-append "0.0.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/stumpwm/stumpwm-contrib.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1dfwsvz1c8w6j4jp0kzaz78ml3f5dp0a5pvf090kwpbpg176r7iq"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir-elisp
             ;; Elisp directory is not in root of the source.
             (lambda _
               (chdir "util/swm-emacs"))))))
      (home-page "https://github.com/stumpwm/stumpwm-contrib")
      (synopsis "Emacs minor-mode for Stumpwm")
      (description "Emacs minor-mode for Stumpwm")
      (license license:gpl3+))))

(define-public emacs-suggest
  (package
    (name "emacs-suggest")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Wilfred/suggest.el/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1760fm3j19w8xxcawq6s859h86q1rdg69pg9yz48n76kwfk3vlgp"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-loop" ,emacs-loop)
       ("emacs-dash" ,emacs-dash)
       ("emacs-s" ,emacs-s)
       ("emacs-f" ,emacs-f)))
    (home-page "https://github.com/Wilfred/suggest.el")
    (synopsis "Suggest Elisp functions that give the output requested")
    (description "Suggest.el will find functions that give the output
requested.  It's a great way of exploring list, string and arithmetic
functions.")
    (license license:gpl3+)))

(define-public emacs-tern
  (package
    (name "emacs-tern")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ternjs/tern/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "1pzchd29i6dxfgm0ackr2vc2xqpczjkwl5h6l8jils0bcfaj48ss"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-elisp
           ;; Elisp directory is not in root of the source.
           (lambda _
             (chdir "emacs"))))))
    (home-page "http://ternjs.net/")
    (synopsis "Tern-powered JavaScript integration")
    (description "Tern-powered JavaScript integration.")
    (license license:gpl3+)))

(define-public emacs-web-beautify
  (package
    (name "emacs-web-beautify")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/yasuyk/web-beautify/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1j57hwid74id4swkx2g0iljfawx0k9c7qjrwqc0mv657x9p78hcs"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/yasuyk/web-beautify")
    (synopsis "Format HTML, CSS and JavaScript/JSON")
    (description "Add the following to your Emacs init file.

    (require 'web-beautify) ;; Not necessary if using ELPA package
    (eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd \"C-c b\") 'web-beautify-js))
    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd \"C-c b\") 'web-beautify-js))
    (eval-after-load 'sgml-mode
      '(define-key html-mode-map (kbd \"C-c b\") 'web-beautify-html))
    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd \"C-c b\") 'web-beautify-css))

If you want to automatically format before saving a file,
add the following hook to your Emacs configuration:

    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    (eval-after-load 'json-mode
      '(add-hook 'json-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    (eval-after-load 'sgml-mode
      '(add-hook 'html-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

    (eval-after-load 'css-mode
      '(add-hook 'css-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

For more information, See URL https://github.com/yasuyk/web-beautify.")
    (license license:gpl3+)))

(define-public emacs-websocket
  (package
    (name "emacs-websocket")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ahyatt/emacs-websocket/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "07nz1spb2nklyf94fdh1rzbmscms9qxc7ypl77fzyvyw3az6qr50"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ahyatt/emacs-websocket")
    (synopsis "Emacs WebSocket client and server")
    (description "This implements RFC 6455, which can be found at
@url{http://tools.ietf.org/html/rfc6455}.

This library contains code to connect Emacs as a client to a websocket server,
and for Emacs to act as a server for websocket connections.

Websockets clients are created by calling @code{websocket-open}, which returns
a @code{websocket} struct.  Users of this library use the websocket struct,
and can call methods @code{websocket-send-text}, which sends text over the
websocket, or @code{websocket-send}, which sends a @code{websocket-frame}
struct, enabling finer control of what is sent.  A callback is passed to
@code{websocket-open} that will retrieve websocket frames called from the
websocket.  Websockets are eventually closed with @code{websocket-close}.

Server functionality is similar.  A server is started with
@code{websocket-server} called with a port and the callbacks to use, which
returns a process.  The process can later be closed with
@code{websocket-server-close}.  A @code{websocket} struct is also created for
every connection, and is exposed through the callbacks.")
    (license license:gpl3+)))

(define-public emacs-wordgen
  (package
    (name "emacs-wordgen")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Fanael/wordgen.el/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "1h2iyixdm49h53pwj9ics9gb9h3g6wa4hainpnjg6mfarf49jkmg"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/wordgen.el")
    (synopsis "Random word generator")
    (description "Generate random words using user-provided rules.

Example:
(wordgen '((result (concat-reeval [(2 1) (5 2) (4 3)] syl))
   (syl (++ c v coda))
   (c [(4 \"p\") (5 \"t\") (5 \"k\") (3 \"m\") (4 \"n\") (3 \"s\") (4
\"l\") (3 \"r\")])
   (v [\"a\" \"e\" \"i\" \"o\" \"u\"])
   (coda [(4 \"\") \"m\" \"n\"]))
 :word-count 5)

=> (\"komlamkim\" \"kepa\" \"mennem\" \"ne\" \"palu\")

See the function `wordgen' for complete description.
")
    (license license:gpl3+)))

(define-public geiser-checkout
  (let ((commit "9581e61f9d8839281fe42344dd5fe885ea7359ea")
        (revision "1"))
    (package
      (inherit geiser)
      (name "geiser")
      (version (string-append "0.9" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.savannah.gnu.org/geiser.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32 "0nx86pzncab0b7700m8sc7k3nzml4v9frrq77lljjcjhw71gnvnn"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("texinfo" ,texinfo)
         ,@(package-native-inputs geiser)))
      (arguments
       (substitute-keyword-arguments
           `(#:parallel-build? #t
                               #:tests? #f
                               ,@(package-arguments geiser))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'autogen
               (lambda _
                 (zero? (system* "sh" "autogen.sh"))))
             (delete 'reset-gzip-timestamps))))))))

(define-public emacs-guix-checkout
  (package
    (inherit emacs-guix)
    (version (string-append (package-version emacs-guix) "-checkout"))
    (propagated-inputs
     `(("geiser" ,geiser-checkout)
       ("dash" ,emacs-dash)
       ("bui" ,emacs-bui)
       ("magit-popup" ,emacs-magit-popup)))))

(define-public emacs-checkout
  (let ((commit "767b3a7429d94d1565256565fda2060c95ca4f73")
        (revision "1"))
    (package
      (inherit emacs)
      (name "emacs")
      (version (string-append (package-version emacs) "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; "git://git.savannah.gnu.org/emacs.git"
               (url "git://localhost/~natsu/src/emacs")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (patches (search-patches "emacs-exec-path.patch"
                                  "emacs-source-date-epoch.patch"))
         (modules '((guix build utils)))
         (snippet
          ;; Delete the bundled byte-compiled elisp files and
          ;; generated autoloads.
          '(with-directory-excursion "lisp"
             (for-each delete-file
                       (append (find-files "." "\\.elc$")
                               (find-files "." "loaddefs\\.el$")))

             ;; Make sure Tramp looks for binaries in the right places on
             ;; remote GuixSD machines, where 'getconf PATH' returns
             ;; something bogus.
             (substitute* "net/tramp-sh.el"
               ;; Patch the line after "(defcustom tramp-remote-path".
               (("\\(tramp-default-remote-path")
                (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
                        "~/.guix-profile/bin" "~/.guix-profile/sbin"
                        "/run/current-system/profile/bin"
                        "/run/current-system/profile/sbin")))))
         (sha256
          (base32 "1kr046v62z1a6sbj6gcibdasbiahyq8zyszv2zzsdq2y8d7yq54g"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("bash" ,bash-minimal)
         ("perl" ,perl)
         ("rc" ,rc)
         ("python" ,python-2.7)
         ("chez-scheme" ,chez-scheme)
         ,@(package-native-inputs emacs)))
      (arguments
       (substitute-keyword-arguments
           `(#:parallel-build? #t
                               #:tests? #f
                               ,@(package-arguments emacs))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'autogen
               (lambda _
                 (zero? (system* "sh" "autogen.sh"))))
             (delete 'reset-gzip-timestamps))))))))

(define-public emacs-indium-checkout
  (let ((commit "d98a9e0cd11d8230c4c3d0b59c4ac60520e34ebb")
        (revision "1"))
    (package
      (inherit emacs-indium)
      (name "emacs-indium")
      (version (string-append (package-version emacs-indium) "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/NicolasPetton/Indium.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1q3yf45fmbjppv3ahb1gdb95pa3kyn18x5m23ihpxz1pziz3a074")))))))

(define-public emacs-know-your-http-well
  (package
    (name "emacs-know-your-http-well")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/for-GET/know-your-http-well/archive/"
             "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1y3kwz88awcgwaivlswq0q4g2i02762r23lpwg61bfqy5lrjjqnj"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-json-files
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (directory)
                         (copy-recursively directory
                                           (string-append
                                            (assoc-ref outputs "out")
                                            directory)))
                       '("js" "json"))))
         (add-after 'unpack 'chdir-elisp
           ;; Elisp directory is not in root of the source.
           (lambda _
             (chdir "emacs"))))))
    (build-system emacs-build-system)
    (home-page "https://github.com/for-GET/know-your-http-well")
    (synopsis "Meaning of HTTP headers codes")
    (description "Meaning of HTTP headers codes.")
    (license license:gpl3+)))

(define-public emacs-company-restclient
  (package
    (name "emacs-company-restclient")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/iquiw/company-restclient/archive/"
             "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1kr3f0wgqlk7r171bvb2kinv7fanwj2md01wdpx04qkgwcr1as00"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-company" ,emacs-company)
       ("emacs-know-your-http-well" ,emacs-know-your-http-well)
       ("emacs-restclient" ,emacs-restclient)))
    (home-page "https://github.com/iquiw/company-restclient")
    (synopsis "Company-mode completion back-end for restclient-mode")
    (description "@code{company-mode} back-end for
@code{restclient-mode}.

It provides auto-completion for HTTP methods and headers in
@code{restclient-mode}.  Completion source is given by
@code{know-your-http-well}.")
    (license license:gpl3+)))

(define-public emacs-helm-notmuch
  (package
    (name "emacs-helm-notmuch")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/xuchunyang/helm-notmuch/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "09b3jllppfmk0mb1qvgcx705jwixqn5ggl0bql6g5a3i7yy6xpyd"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("notmuch" ,notmuch)
       ("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/xuchunyang/helm-notmuch")
    (synopsis "Search emails with Notmuch and Helm")
    (description "Search emails, searching result displays as you type
thanks to helm.")
    (license license:gpl3+)))
