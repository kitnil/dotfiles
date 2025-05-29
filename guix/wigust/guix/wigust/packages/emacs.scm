;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020, 2021, 2022, 2023 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (guix hg-download)
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
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages libevent)
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
  #:use-module (gnu packages password-utils)
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
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix utils)
  #:use-module (guix build emacs-utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public emacs-with-patched-tramp
  (package
    (inherit emacs)
    (source
     (origin
       (inherit (package-source emacs))
       (patches (fold cons* '()
                      (origin-patches (package-source emacs))
                      (search-patches "emacs-tramp-sudo.patch")))))))

;; TODO: $out/bin/emacs point to empty file in emacs-athena package.
(define-public emacs-athena
  ;; GTK+ could kill emacs --daemon,
  ;; see <https://bugzilla.gnome.org/show_bug.cgi?id=85715>.
  (package
    (inherit emacs)
    (name "emacs-athena")
    (synopsis "The extensible, customizable, self-documenting text
editor with athena toolkit" )
    (build-system gnu-build-system)
    (inputs `(("libxaw" ,libxaw)
              ("emacs-texinfo-keybindgs"
               ,(origin
                  (method url-fetch)
                  (uri "https://git.savannah.gnu.org/cgit/emacs.git/patch/lisp/textmodes/texinfo.el?id=05bffa1f0e3e04a501801d8e7417b623ac78a584")
                  (sha256
                   (base32
                    "148gmlkgkibc1211xxk738h7xglbsig9q21rg4bipdk2vii63n20"))
                  (file-name "emacs-texinfo-keybindgs.patch")))
              ,@(alist-delete "gtk+" (package-inputs emacs))))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-elisp
             (lambda* (#:key inputs #:allow-other-keys)
               (invoke "patch" "-p1" "--input" (assoc-ref inputs "emacs-texinfo-keybindgs"))))
           (delete 'restore-emacs-pdmp)))
       ((#:configure-flags flags)
        `(cons "--with-x-toolkit=athena" ,flags))))))

(define-public emacs-elfeed-custom
  (package
    (inherit emacs-elfeed)
    (name "emacs-elfeed-custom")
    (source
     (origin
       (inherit (package-source emacs-elfeed))
       (patches (fold cons* '()
                      (origin-patches (package-source emacs-elfeed))
                      (search-patches
                       "emacs-elfeed-search-fix-browse-at-remote.patch")))))))

(define-public emacs-elfeed-summary
  (package
    (name "emacs-elfeed-summary")
    (version "20220331.1634")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SqrtMinusOne/elfeed-summary.git")
             (commit "517cea6cb6fbf95ef3cb062591364bb7bda8c251")))
       (sha256
        (base32 "17ir0kmb6v9fyl6ink2mc25fxf0hs2kbcpfaxwcr8kz087n2bz7n"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-magit emacs-elfeed))
    (home-page "https://github.com/SqrtMinusOne/elfeed-summary.el")
    (synopsis "Feed summary interface for elfeed")
    (description
     "The package provides a tree-based feed summary interface for elfeed.  The tree
can include individual feeds, searches, and groups.  It mainly serves as an
easier \"jumping point\" for elfeed, so searching a subset of the elfeed database
is one action away.

`elfeed-summary' pops up the summary buffer.  The buffer shows individual feeds
and searches, combined into groups.  The structure is determined by the
`elfeed-summary-settings' variable.

Also take a look at the package README at
<https://github.com/SqrtMinusOne/elfeed-summary> for more information.")
    (license #f)))

(define-public emacs-company-tern
  (package
    (name "emacs-company-tern")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/proofit404/company-tern/archive/"
             "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
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
    (description "This package provides Tern backend for Company.")
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
     `(("emacs-js2-mode" ,emacs-js2-mode)
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
       `(("pandoc" ,ghc-pandoc)
         ("emacs-s" ,emacs-s)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'install-shell-script
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "org-protocol-capture-html.sh"
                             (string-append (assoc-ref outputs "out")
                                            "/bin")))))))
      (home-page "https://github.com/alphapapa/org-protocol-capture-html")
      (synopsis "Captures Web pages into Org using Pandoc to process HTML")
      (description "This package captures Web pages into Org-mode using Pandoc to
process HTML.  It can also use eww's eww-readable functionality to
get the main content of a page.

These are the helper functions that run in Emacs.  To capture pages
into Emacs, you can use either a browser bookmarklet or the
org-protocol-capture-html.sh shell script.  See the README.org file
for instructions.")
      (license license:gpl3+))))

(define-public emacs-rjsx-mode
  (package
    (name "emacs-rjsx-mode")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/felipeochoa/rjsx-mode/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0n43wm7s19csbnk2gsqpnb6pcywa7l51rx5z9d35x13bm9f3q0ap"))))
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

(define-public emacs-tern
  (package
    (name "emacs-tern")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ternjs/tern/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pzchd29i6dxfgm0ackr2vc2xqpczjkwl5h6l8jils0bcfaj48ss"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-auto-complete" ,emacs-auto-complete)))
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

(define-public emacs-helm-notmuch
  (package
    (name "emacs-helm-notmuch")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/xuchunyang/helm-notmuch/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09b3jllppfmk0mb1qvgcx705jwixqn5ggl0bql6g5a3i7yy6xpyd"))))
    (build-system emacs-build-system)
    ;; TODO: helm-notmuch.el:36:1:Error: Cannot open load file:
    ;; No such file or directory, notmuch
    (propagated-inputs
     `(("notmuch" ,notmuch)
       ("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/xuchunyang/helm-notmuch")
    (synopsis "Search emails with Notmuch and Helm")
    (description
     "Search emails, searching result displays as you type thanks to helm.")
    (license license:gpl3+)))

(define-public emacs-js3-mode
  (let ((commit "229aeb374f1b1f3ee5c59b8ba3eebb6385c232cb"))
    (package
      (name "emacs-js3-mode")
      (version (git-version "1.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thomblake/js3-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0yd2lck1kq01pxk86jpxff5ih6fxx1a1wvl7v8b5ys7gi33fjqz2"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/thomblake/js3-mode")
      (synopsis "Improved JavaScript editing mode")
      (description
       "This package provides a JavaScript editing mode for Emacs.")
      (license license:gpl3+))))

(define-public emacs-js-comint
  (package
    (name "emacs-js-comint")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/redguardtoo/js-comint/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "0prsmz274i3891w5ppp9wzf4q7jwjsi6yaainwpcpiwx23qckrr6"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/redguardtoo/js-comint")
    (synopsis "JavaScript interpreter in Emacs window")
    (description "This package provides a program is a comint mode for Emacs
which allows you to run a compatible javascript REPL like Node.js,
Spidermonkey, Rhino inside Emacs.  It also defines a few functions for sending
JavaScript input to it quickly.")
    (license license:gpl3+)))

(define-public emacs-eval-in-repl
  (package
    (name "emacs-eval-in-repl")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/kaz-yos/eval-in-repl/archive/"
             "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lw97llr0lzipbi4a7q7qrjvqz9g7bip46rkm88sgbagh218sjr4"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-paredit" ,emacs-paredit)
       ("emacs-ace-window" ,emacs-ace-window)))
    (inputs
     `(("emacs-slime" ,emacs-slime)
       ("emacs-sml-mode" ,emacs-sml-mode)
       ("emacs-cider" ,emacs-cider)
       ("geiser" ,emacs-geiser)
       ("emacs-lua-mode" ,emacs-lua-mode)
       ("emacs-js2-mode" ,emacs-js2-mode)
       ("emacs-js3-mode" ,emacs-js3-mode)
       ("emacs-js-comint" ,emacs-js-comint)
       ("emacs-inf-ruby" ,emacs-inf-ruby)
       ("emacs-tuareg" ,emacs-tuareg)
       ("emacs-hy-mode" ,emacs-hy-mode)
       ("emacs-racket-mode" ,emacs-racket-mode)))
    (home-page "https://github.com/kaz-yos/eval-in-repl/")
    (synopsis "Consistent ESS-like eval interface for various REPLs")
    (description "Consistent ESS-like eval interface for various REPLs")
    (license license:gpl3+)))

(define-public emacs-assess
  (package
    (name "emacs-assess")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/phillord/assess.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15ismdk8fl6xjgkjh9nkn8kgg7rpy2ra7924r1iddlfs2q8l8p4w"))))
    (inputs
     `(("emacs-m-buffer-el" ,emacs-m-buffer-el)
       ("emacs-load-relative" ,emacs-load-relative)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "emacs" "--batch" "-L" "."
                     "-L" (string-append
                           (assoc-ref inputs "emacs-m-buffer-el")
                           "/share/emacs/site-lisp/guix.d/m-buffer-el-"
                           ,(package-version emacs-m-buffer-el))
                     "--load" "assess-discover.el"
                     "--eval" "(assess-discover-run-and-exit-batch t)"))))))
    (build-system emacs-build-system)
    (home-page "https://github.com/phillord/assess")
    (synopsis "Test support functions for Emacs")
    (description "@code{assess} provides additional support for
testing Emacs packages.

It provides:

@itemize
@item A set of predicates for comparing strings, buffers and file
contents.
@item Explainer functions for all predicates giving useful output.
@item Macros for creating many temporary buffers at once, and for
restoring the buffer list.
@item Methods for testing indentation, by comparision or
roundtripping.
@item Methods for testing fontification.
@item Assess aims to be a stateless as possible, leaving Emacs
unchanged whether the tests succeed or fail, with respect to buffers,
open files and so on.  This helps to keep tests independent from each
other.
@end itemize\n")
    (license license:gpl3+)))

(define-public emacs-auth-password-store
  (package
    (name "emacs-auth-password-store")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/DamienCassou"
                           "/auth-password-store"
                           "/archive/" "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n9623hsx6vsq87y6a2731ydfi0x8fvfb6m7fbdfy726d4pnr09q"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-dash" ,emacs-dash)
       ("emacs-f" ,emacs-f)
       ("emacs-s" ,emacs-s)
       ("emacs-password-store" ,emacs-password-store)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "emacs" "--batch" "-L" "."
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-password-store")
                                   "/share/emacs/site-lisp/guix.d/password-store-"
                                   ,(package-version emacs-password-store))
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-f")
                                   "/share/emacs/site-lisp/guix.d/f-"
                                   ,(package-version emacs-f))
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-s")
                                   "/share/emacs/site-lisp/guix.d/s-"
                                   ,(package-version emacs-s))
                             "-L" (string-append
                                   (assoc-ref inputs "emacs-dash")
                                   "/share/emacs/site-lisp/guix.d/dash-"
                                   ,(package-version emacs-dash))
                             "-l" "test/auth-password-store-tests.el"
                             "-f" "ert-run-tests-batch-and-exit")))))))
    (home-page "https://github.com/DamienCassou/auth-password-store")
    (synopsis "Integrate Emacs auth-source with password-store")
    (description "@code{emacs-auth-password-store} integrates Emacs
auth-source library with @code{password-store}.")
    (license license:gpl3+)))

(define-public emacs-xml-rpc
  (package
    (name "emacs-xml-rpc")
    (version "1.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hexmode/xml-rpc-el/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01izmsxk0ja77nxwgjzyb3k5wgndnx7i9qvkyb8f12bwrjn0rga0"))))
    (build-system emacs-build-system)
    ;; TODO: Package tests
    (home-page "https://github.com/hexmode/xml-rpc-el")
    (synopsis "XML-RPC client implementation in elisp")
    (description "This package provides an XML-RPC client
implementation in elisp, capable of both synchronous and asynchronous
method calls.")
    (license license:gpl3+)))

(define emacs-wi-web-search
  (let ((commit "33fa377156f487e41deda62cd92110d707907c66"))
    (package
      (name "emacs-wi-web-search")
      (version (string-append "0.0.1" "-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wigust/.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "04lxmd0h7mfjjl0qghrycgff0vcv950j1wqv0dbkr61jxp64n5fv"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/wigust/emacs-wi-web-search")
      (synopsis "Emacs function to search in web")
      (description "@code{wi-web-search} provides the Emacs function
to search in web.")
      (license license:gpl3+))))

;; TODO: `M-x mastodon': json-read: End of file while parsing JSON
(define-public emacs-mastodon
  (let ((commit "e08bb5794762d22f90e85fd65cef7c143e6b9318")
        (revision "1"))
    (package
      (name "emacs-mastodon")
      (version (string-append "0.7.1" "-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jdenen/mastodon.el.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0bil0xxava04pd4acjqm3bfqm1kjdk4g0czd4zqvacsp5c9sl2qp"))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir-elisp
             ;; Elisp directory is not in root of the source.
             (lambda _
               (chdir "lisp")))
           ;; TODO: Tests:
           ;; (add-before 'install 'check
           ;;   (lambda _
           ;;     (with-directory-excursion ".."
           ;;       (zero? (system* "emacs" "--batch" "-L" "lisp"
           ;;                     "-l" "test/ert-helper.el"
           ;;                     "-f" "ert-run-tests-batch-and-exit")))))
           )))
      (build-system emacs-build-system)
      (home-page "https://github.com/jdenen/mastodon.el")
      (synopsis "Emacs client for Mastodon social network")
      (description "This package provides an Emacs client for
Mastodon federated social network.")
      (license license:gpl3+))))

(define-public emacs-engine-mode-autoload
  (package
    (inherit emacs-engine-mode)
    (name "emacs-engine-mode-autoload")
    (source
     (origin
       (inherit (package-source emacs-engine-mode))
       (modules '((guix build utils)))
       (snippet
        '(substitute* "engine-mode.el"
           (("\\(cl-defmacro defengine" line)
            (string-append ";;;###autoload\n" line))))))))

(define-public emacs-strace-mode-special
  (package
    (inherit emacs-strace-mode)
    (name "emacs-strace-mode-special")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "strace-mode.el"
               (("fundamental-mode") "special-mode")))))))))

(define-public emacs-lognav-mode
  (let ((changeset "a9b53f2da040")
        (revision "1"))
      (package
    (name "emacs-lognav-mode")
    (version (string-append "0.0.5-" revision "." changeset))
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "https://bitbucket.org/ellisvelo/lognav-mode")
                    (changeset changeset)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0ka92x63zfgraby5ycypn3ri2s3s2b1m2j7fpb4r37bk9xkf90v4"))))
    (build-system emacs-build-system)
    (home-page "https://bitbucket.org/ellisvelo/lognav-mode")
    (synopsis "Navigate log error messages in Emacs")
    (description "Lognav-mode is a minor mode used for finding and navigating
errors within a buffer or a log file.  For example, M-n moves the cursor to
the first error within the log file.  M-p moves the cursor to the previous
error.  Lognav-mode only highlights the errors that are visible on the screen
rather than highlighting all errors found within the buffer.  This is
especially useful when opening up large log files for analysis.")
    (license license:gpl2+))))

(define-public emacs-terminal-here
  (let ((commit "b3659e13d3d41503b4fc59dd2c7ea622631fc3ec")
        (revision "1"))
    (package
      (name "emacs-terminal-here")
      (version (string-append "1.0-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/davidshepherd7/terminal-here.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1z3ngwgv8ybwq42lkpavk51a25zdkl6v9xdfi41njpxdpbfcmx8z"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/davidshepherd7/terminal-here")
      (synopsis "Run an external terminal in current directory")
      (description "Provides commands to help open external terminal emulators
in the directory of the current buffer.")
      (license license:gpl3+))))

(define-public emacs-info-colors
  (package
    (name "emacs-info-colors")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ubolonton/info-colors/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08xd6y89igxlqpw678xjpxyzs9k28vbbc7sygxcyblgyj6farnml"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ubolonton/info-colors")
    (synopsis "Extra colors for Info-mode")
    (description "This package provides a modern adaption of the extra
coloring provided by Drew Adams @code{info+} package.  To enable this
@code{(add-hook 'Info-selection-hook 'info-colors-fontify-node)}.")
    (license license:gpl3+)))

(define-public emacs-origami
  (let ((commit "1f38085c8f9af7842765ed63f7d6dfe4dab59366")
        (revision "1"))
    (package
      (name "emacs-origami")
      (version (string-append "0.1" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gregsexton/origami.el.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0ha1qsz2p36pqa0sa2sp83lspbgx5lr7930qxnwd585liajzdd9x"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-s" ,emacs-s)
         ("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/gregsexton/origami.el")
      (synopsis "Folding minor mode for Emacs")
      (description
       "This package provides a text folding minor mode for Emacs.")
      (license license:expat))))

(define-public emacs-srfi
  (package
    (name "emacs-srfi")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://emacswiki.org/emacs/download/srfi.el"))
       (file-name (string-append "emacs-srfi-" version ".el"))
       (sha256
        (base32
         "05xh6jdgds79xchm01s2r04sv61gglf4v33f1968yw1zfi9g5jhi"))))
    (build-system emacs-build-system)
    (home-page "https://www.emacswiki.org/emacs/srfi.el")
    (synopsis "View Scheme requests for implementation")
    (description "This file provides a functionality to view SRFIs,
Scheme Requests for Implementation, using a simple @code{srfi} command.
To update the local SRFI cache, use @code{srfi-update-cache}.")
    (license license:gpl2+)))

(define-public emacs-awk-it
  (package
    (name "emacs-awk-it")
    (version "0.77")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://emacswiki.org/emacs/download/awk-it.el"))
       (file-name (string-append "emacs-awk-it-" version ".el"))
       (sha256
        (base32
         "1r1vbi1r3rdbkyb2naciqwja7hxigjhqfxsfcinnygabsi7fw9aw"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-awk-it-file-first-line
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((file "emacs-awk-it.el"))
               (chmod file #o644)
               (emacs-substitute-sexps file
                 ("(defcustom awk-it-file-first-line"
                  (string-append "#!" (assoc-ref inputs "gawk") "/bin/awk"))))
             #t)))))
    (propagated-inputs
     `(("emacs-yasnippet" ,emacs-yasnippet)))
    (inputs
     `(("gawk" ,gawk)))
    (home-page "https://www.emacswiki.org/emacs/awk-it.el")
    (synopsis "Run AWK interactively on region")
    (description "AWK it! allows you to see AWK output as you are typing the
script; it sends selected region to awk and uses yasnippet as interactive UI.

There are 3 modes of AWK code: simplified syntax(default, see below),
single line AWK syntax (regular AWK syntax but only inside the default
match) and raw AWK syntax(full AWK code).  AWK it! can transfrom code
from one mode to another(not perfect, but it will make an effort) and
there is also support for multiple lines.  Data is expanded with
selected yasnippet expand keybinding.")
    (license license:gpl2+)))

(define-public emacs-helm-navi
  (let ((commit "2256591174ff79f889450fdc10822316819d6476"))
    (package
      (name "emacs-helm-navi")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-helm/helm-navi.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bbb52v1c81a6ap10qi7mqigi237vwrlmf8mg3ckglm1i710636j"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-navi-mode" ,emacs-navi-mode)
         ("emacs-s" ,emacs-s)))
      (home-page "https://github.com/emacs-helm/helm-navi")
      (synopsis "Navigate through a buffer using the headings and keywords")
      (description
       "This file provides commands to navigate a buffer using keywords and
headings provided by @code{navi-mode} and @code{outshine}.")
      (license license:gpl3+))))

(define-public emacs-git-messenger-diff-mode
  (package
    (inherit emacs-git-messenger)
    (name "emacs-git-messenger-diff-mode")
    (source
     (origin
       (inherit (package-source emacs-git-messenger))
       (modules '((guix build utils)))
         (snippet
          '(substitute* "git-messenger.el"
             (("\\(fundamental-mode\\)") "(diff-mode)")))))))

(define-public emacs-atomic-chrome
  (let ((commit "4828a29855f4663add5f2075b7d874354e70c02c"))
    (package
      (name "emacs-atomic-chrome")
      (version (git-version "2.0.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alpha22jp/atomic-chrome.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "15yg8752z3iwizja7wkjvkjrj8pig21ynq5l5m5cr3f1bzx74dx7"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-let-alist" ,emacs-let-alist)
         ("emacs-websocket" ,emacs-websocket)))
      (home-page "https://github.com/alpha22jp/atomic-chrome/")
      (synopsis "Edit text area on Chrome with Emacs using Atomic Chrome")
      (description "This package provides an Emacs version of Atomic Chrome
which is an extension for Google Chrome browser that allows you to edit text
areas of the browser in Emacs.  It's similar to Edit with Emacs, but has some
advantages as below with the help of websocket.")
      (license license:gpl2+))))

(define-public emacs-perl-live
  (package
    (name "emacs-perl-live")
    (version "1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/vividsnow/perl-live/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0i1ila542bgfpb5r1w62w5bnhh97mpp9mwpjbfp3kr8qn1ymvqq4"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-perl-live-pl
           ;; This build phase installs ‘perl-live.pl’ file
           ;; and patches a location to this in ‘perl-live.el’.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((data (string-append (assoc-ref outputs "out") "/share/"
                                        ,name "-" ,version)))
               (install-file "perl-live.pl" data)
               (let ((file "perl-live.el"))
                 (chmod file #o644)
                 (emacs-substitute-sexps file
                   ("(defcustom perl-live-bin"
                    (which "perl"))
                   ("(defcustom perl-live-script"
                    (string-append data "/perl-live.pl"))))))))))
    (native-inputs
     `(("perl" ,perl)))
    (propagated-inputs
     `(("perl-anyevent" ,perl-anyevent)
       ("perl-package-stash-xs" ,perl-package-stash-xs)
       ("perl-ev" ,perl-ev) ;optional
       ("perl-padwalker" ,perl-padwalker)))
    (home-page "https://github.com/vividsnow/perl-live/")
    (synopsis "Perl live coding")
    (description "This package provides a Perl script for live coding.")
    (license license:gpl3+)))

(define-public emacs-helm-lines
  (let ((commit "4c8d648a2b56e9de79c3199af22e80afe3c01ff5"))
    (package
      (name "emacs-helm-lines")
      (version (git-version "0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/torgeir/helm-lines.el.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pxnpplrg1pzs8jgl4ww3hkl2d7r3ixxyjz8f1112634l35ics7c"))))
      (propagated-inputs `(("emacs-helm" ,emacs-helm)))
      (build-system emacs-build-system)
      (home-page "https://github.com/torgeir/helm-lines.el")
      (synopsis "Helm interface for completing by lines elsewhere in a project")
      (description "This package provides a Helm interface for completing by
lines elsewhere in a project.")
      (license license:gpl3+))))

(define-public emacs-mediawiki
  (let ((commit "8473e12d1839f5287a4227586bf117dad820f867"))
    (package
      (name "emacs-mediawiki")
      (version (git-version "2.2.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hexmode/mediawiki-el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "03rpj3yrk3i1l9yjnamnx38idn6y4zi9zg53bc83sx3g2b4m5v04"))))
      (build-system emacs-build-system)
      (home-page
       "https://github.com/hexmode/mediawiki-el")
      (synopsis "Mediawiki Emacs frontend")
      (description "This package provides a Mediawiki Emacs frontend.")
      (license license:gpl3+))))

(define-public emacs-flyspell-correct
  (let ((commit "0486912f57ac2ec70c472b776c63360462cb32d7"))
    (package
      (name "emacs-flyspell-correct")
      (version (git-version "0.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/d12frosted/flyspell-correct.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0qji0bvm14ra4xjlzx1ww4d0ih752j641n3vms1hh12n439bn6vh"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)
         ("emacs-ivy" ,emacs-ivy)))
      (home-page "https://github.com/d12frosted/flyspell-correct")
      (synopsis "Correct words with @code{flyspell} via custom interface")
      (description "This package provides functionality for correcting words
via custom interfaces.")
      (license license:gpl3+))))

(define-public emacs-proc-net
  (let ((commit "10861112a1f3994c8e6374d6c5bb5d734cfeaf73"))
    (package
      (name "emacs-proc-net")
      (version (git-version "0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nicferrier/emacs-proc-net.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0nly5h0d6w8dc08ifb2fiqcn4cqcn9crkh2wn0jzlz4zd2x75qrb"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/nicferrier/emacs-proc-net")
      (synopsis "Network process tools")
      (description "This package provides an Emacs tools for doing stuff with
network processes.")
      (license license:gpl3+))))

(define-public emacs-build-farm
  (let ((commit "0e0168d75dba589e58d3034d6d865384ee551d86"))
    (package
      (name "emacs-build-farm")
      (version "0.0.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alezost/build-farm.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07bp00lyl6q175d93p71wa93ygkm5q6x3xhw1a53vqmsvkyahqps"))))
      (build-system emacs-build-system)
      (inputs
       `(("emacs-bui" ,emacs-bui)
         ("emacs-magit-popup" ,emacs-magit-popup)))
      (home-page "https://github.com/alezost/build-farm.el")
      (synopsis " Interface for Hydra and Cuirass (Nix and Guix build farms)")
      (description "Emacs-Build-Farm is an Emacs interface for Hydra and Cuirass
— build farms for Nix and Guix package managers.")
      (license license:gpl3+))))

(define-public emacs-gited
  (package
    (name "emacs-gited")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/gited-"
                                  version ".tar"))
              (sha256
               (base32
                "1bayfclczdzrmay8swszs8lliz5p4nnmjzzz2gh68rc16isjgh2z"))))
    (build-system emacs-build-system)
    (home-page "https://elpa.gnu.org/packages/gited.html")
    (synopsis "Operate on Git branches like dired")
    (description "This Emacs library lists the branches in a Git repository.
Then you can operate on them with a dired-like interface.")
    (license license:gpl3+)))

(define-public emacs-psysh
  (package
    (name "emacs-psysh")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/emacs-php/psysh.el/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0h019yarfgwyvfjsvif1l404a9a9j6jlzyfykyahf7j5850k4fhk"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-s" ,emacs-s)
       ("emacs-f" ,emacs-f)))
    (home-page "https://github.com/zonuexe/psysh.el")
    (synopsis "PsySH, PHP interactive shell (REPL)")
    (description
     "This package provides a PHP interactive shell.")
    (license license:gpl3+)))

(define-public emacs-anywhere-mode
  (let ((commit "80a5aa81b7102d27f83e67fb361388a2c80dbc88"))
    (package
      (name "emacs-anywhere-mode")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/wigust/emacs-anywhere-mode.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0avqg4abv7fngwn4vpphf8p6dvasglllrdibhv7rdswg11dpbs22"))))
      (build-system emacs-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-paths
             (lambda _
               (substitute* "emacs-anywhere"
                 (("@XDOTOOL_BIN@") (which "xdotool")))
               (substitute* "anywhere-mode.el"
                   (("@XCLIP_BIN@") (which "xclip")))))
           (add-before 'install 'install-shell-script
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "emacs-anywhere"
                             (string-append (assoc-ref outputs "out")
                                            "/bin")))))))
      (inputs
       `(("xdotool" ,xdotool)
         ("xclip" ,xclip)))
      (synopsis "Emacs Anywhere mode")
      (description "This package provides an Emacs minor-mode for
capturing user input and paste it with @kbd{C-v} after exit.")
      (home-page #f)
      (license license:gpl3+))))

(define-public emacs-redshift
  (let ((commit "6d0501933b0faff53bcc431a0444675fc896a396"))
    (package
      (name "emacs-redshift")
      (version (git-version "0.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/wigust/emacs-redshift")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1dlwibn5az96yb3vi62a0r6g8d8d5dn5bkkrgrr4w3qn9j64pv3z"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/wigust/emacs-redshift")
      (synopsis "Emacs interface to Redshift")
      (description
       "This package provides an Emacs interface to Redshift")
      (license license:gpl3+))))

(define-public emacs-terminal-here-checkout
  (let ((commit "ca1132ae5ea76de9e7122d61eb96f3a95fcce04f"))
    (package
      (inherit emacs-terminal-here)
      (name "emacs-terminal-here-checkout")
      (version (git-version "1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wigust/terminal-here")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jixxp62d3bvl1dmn6bbq0gfpb9axccynnfffc0x99ii1ak2hf96"))))
      (propagated-inputs
       `(("emacs-s" ,emacs-s)
         ,@(package-propagated-inputs emacs-terminal-here))))))

(define-public emacs-hydra-timestamp
  (let ((commit "49b029193be57eafe542247e42b28a86fd34cdab"))
    (package
      (name "emacs-hydra-timestamp")
      (version (string-append "0.1" "-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/wigust/emacs-hydra-timestamp")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0pyqsmz3144c6avy075bxddv7czwar9g0rzzgm62saxywfwxjxm8"))))
      (build-system emacs-build-system)
      (synopsis "Insert timestamps with Emacs Hydra")
      (description
       "This package provides an Emacs Hydra for inserting timestamps.")
      (home-page #f)
      (license license:gpl3+))))

(define-public emacs-guix-misc
  (let ((commit "6eb52ef6a320a9f535f665b3b400b66448dbeb6f"))
    (package
      (name "emacs-guix-misc")
      (version (string-append "0.0.1" "-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/wigust/emacs-guix-misc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "19hihkd12gp3mb6fd5bfkkv369ljbm7389g8mik52rdvdn4v1v3j"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir-elisp
             ;; Elisp directory is not in root of the source.
             (lambda _
               (chdir "lisp"))))))
      (home-page "https://github.com/wigust/emacs-guix-misc")
      (synopsis "Additional functions for Emacs Guix")
      (description
       "This package provides an additional functionality for Emacs Guix.")
      (license license:gpl3+))))

(define-public emacs-buffer-move
  (package
    (name "emacs-buffer-move")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lukhas/buffer-move.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xdks4jfqyhkh34y48iq3gz8swp0f526kwnaai5mhgvazvs4za8c"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/lukhas/buffer-move")
    (synopsis "Easily swap Emacs buffers")
    (description "This package provides a way to swap Emacs buffers without
typing C-x b on each window.")
    (license license:gpl3+)))

(define-public emacs-symbol-overlay
  (let ((commit "11f91ce5b6e5b15f6fef364a701880144ddca6e2"))
    (package
      (name "emacs-symbol-overlay")
      (version (git-version "4.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/wolray/symbol-overlay.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vj424bbgwbsn690qhfbvhla3nqfz22v4ml1ykafxak3nvkm4zxd"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/wolray/symbol-overlay/")
      (synopsis "Highlight symbols with keymap-enabled overlays")
      (description
       "Highlighting symbols with overlays while providing a keymap for
various operations about highlighted symbols.  It was originally inspired by
the package @code{highlight-symbol}.  The fundamental difference is that in
@code{symbol-overlay} every symbol is highlighted by the Emacs built-in
function @code{overlay-put} rather than the @code{font-lock} mechanism used in
@code{highlight-symbol}.")
      (license license:gpl3+))))

(define-public emacs-whois
  (let ((commit "3c3c41b12c7a8ea948d268ebc804ad6f4d66b8a1"))
    (package
      (name "emacs-whois")
      (version (git-version "0.0.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lassik/emacs-whois.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mpzmpdgq92pkyc0lijjvbivcg8hb6wlbppyilmlc1rizl6crakv"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/lassik/emacs-whois/")
      (synopsis "Extends the whois functionality of Emacs")
      (description "This package complements (does not replace) the standard
whois functionality of GNU Emacs.  It provides:

@itemize
@item whois-mode with font-lock highlighting to make whois responses easier to
read.
@item whois-shell function to make a whois query using the system whois
program instead of Emacs' own (often not up to date) whois client.
@end itemize\n")
      (license license:gpl3+))))

(define-public emacs-helm-suggest
  (let ((commit "7e9687fdbb69312dc5e79630b700c0ce1be8441b"))
    (package
      (name "emacs-helm-suggest")
      (version (git-version "0.1" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xuchunyang/helm-suggest.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jmrcdm980zqmihlss53a4p6k6h4lapsb70lnk9sfcx3cqpmn912"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-helm" ,emacs-helm)))
      (home-page "https://github.com/xuchunyang/helm-suggest/")
      (synopsis "Web suggest with Emacs Helm")
      (description "This package provides web suggestions with Emacs Helm.")
      (license license:gpl3+))))

(define-public emacs-helm-tramp
  (package
    (name "emacs-helm-tramp")
    (version "1.3.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/masasam/emacs-helm-tramp.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19y97ia4zl50wd22gkb7kkhj713imd5jc92l2xnpw04b59b60di1"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-helm" ,emacs-helm)))
    (home-page "https://github.com/masasam/emacs-helm-tramp/")
    (synopsis "Tramp Helm interface for SSH server, Docker and Vagrant")
    (description "This package provides Tramp Helm interface for SSH server,
Docker and Vagrant.")
    (license license:gpl3+)))

(define-public emacs-jenkins
  (let ((commit "9c7b9d4cb39eff7d6ac4d0cbd5ebc103dc86cac2"))
    (package
      (name "emacs-jenkins")
      (version (git-version "0.1" "1" commit)) ;no upstream release
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rmuslimov/jenkins.el.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1s49xmpc975q1ffb5c7cjrcwpa4a7kcakri26f5vqy3hn1h0c2v7"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/rmuslimov/jenkins.el/")
      (synopsis "Jenkins client for Emacs")
      (description "This package provides a Jenkins client for Emacs.")
      (license license:gpl3+))))

(define-public emacs-slack-patched
  (let ((commit "ea89ac4291532a136d02bb8852b5dc641622ab73"))
    (package
      (inherit emacs-slack)
      (name "emacs-slack-patched")
      (version (git-version "0.0.2" "8" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yuya373/emacs-slack.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0gnmhlv3gzv5n8ydbg84n9m6i9d0akcvn032ipsyss6bqw1vzl1m"))))
      (arguments
       (substitute-keyword-arguments
           (package-arguments emacs-slack)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch
               (lambda _
                 (substitute* "slack-message-buffer.el"
                   (("slack-conversations-view") "slack-conversations-history")))))))))))

(define-public emacs-copy-as-format
  (package
    (name "emacs-copy-as-format")
    (version "0.0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sshaw/copy-as-format.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i158bkra7zgq75j08knq2camvlhbs2v8zrsxiyp0mc4q949xysd"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/sshaw/copy-as-format/")
    (synopsis "Copy buffer locations as GitHub/Slack/JIRA as formatted code")
    (description "Copy buffer locations as GitHub/Slack/JIRA/HipChat formatted
code and add them to the kill ring.  The buffer will not be modified.

With a prefix argument prompt for the format.  Defaults to
@code{copy-as-format-default}.

To add formats see @code{copy-as-format-format-alist}.

For AsciiDoc customization see
@code{copy-as-format-asciidoc-include-file-name} and
@code{copy-as-format-asciidoc-language-alist}.")
    (license license:gpl3+)))

(define-public emacs-tramp-auto-auth-my
  (let ((commit "21bf0950f03e13d0a442aac6c40ae4e7c51531b3"))
    (package
      (inherit emacs-tramp-auto-auth)
      (name "emacs-tramp-auto-auth-my")
      (version (git-version "20191027" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wigust/tramp-auto-auth.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "18brdnhg2k0dqpkd2rfr0wvqiggmq1k27y89sjld670pb4v1gjin")))))))

(define-public emacs-apache-mode
  (let ((commit "354f9302a8d805ac80d846adcd1cef10830b3d51"))
    (package
      (name "emacs-apache-mode")
      (version "2.2.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-php/apache-mode.git")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1grs2x51k8pa6sgfa82s5pwwdfv7zw46ccw0zvd2rvzbhpq21p2z"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/emacs-php/apache-mode/")
      (synopsis "Major mode for editing Apache configuration files")
      (description "This package provides an Emacs major mode for editing
Apache configuration files.")
      (license license:gpl2+))))

(define-public emacs-highlight-indent-guides
  (let ((commit "0b10f38c54ffc099861ce8463e16a1b07ddbb203"))
    (package
      (name "emacs-highlight-indent-guides")
      (version (git-version "20190108" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DarthFennec/highlight-indent-guides.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0fa41y9gdci60417zksj9bgjx1knwh781i3dfphkgr7qbx25iky7"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/DarthFennec/highlight-indent-guides/")
      (synopsis "Minor mode to highlight indentation")
      (description "This minor mode highlights indentation levels via font-lock.")
      (license license:gpl3+))))

(define-public emacs-plain-org-wiki
  (let ((commit "887717c184fb22dd219c78851303a8e5b917f877"))
    (package
      (name "emacs-plain-org-wiki")
      (version "20191013.1833")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/abo-abo/plain-org-wiki.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wqxp46zvwswda644mnr92qxyyvakvl2l8w61sg6hy37pwfznx73"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-ivy" ,emacs-ivy)))
      (home-page "https://github.com/abo-abo/plain-org-wiki")
      (synopsis "Simple jump-to-org-files in a directory package")
      (description "Call @code{plain-org-wiki} to either open your Org files, or
create new ones.")
      (license license:gpl3+))))

(define-public emacs-ytel
  (let ((commit "828323f9acb654d8f61ccbca510b471fb00879ee"))
    (package
      (name "emacs-ytel")
      (version (git-version "0.1.0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gRastello/ytel.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zixqicpdywzdc2pkx9h08y3kphmn9zcx44bj2rqg3h8xsmjayk5"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/gRastello/ytel/")
      (synopsis "Query YouTube via Invidious")
      (description "This package provide a major mode to search YouTube videos via an elfeed-like
buffer.  Information about videos displayed in this buffer can be extracted
and manipulated by user-defined functions to do various things such as:
- playing them in some video player
- download them")
      (license license:gpl3+))))

(define-public emacs-dashboard-with-agenda-reverse
  (let ((commit "dbb6eb420a6b1909fe02c5eaeebfd17ea64297f3"))
    (package
      (inherit emacs-dashboard)
      (name "emacs-dashboard-with-agenda-reverse")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wigust/emacs-dashboard.git")
                      (commit commit)))
                (file-name (git-file-name name (package-version emacs-dashboard)))
                (sha256
                 (base32
                  "1svnsg8lh4v39wkkgc250qa3lcknma17mgd080vf12641r9d847n")))))))

(define-public emacs-stupid-indent-mode
  (package
    (name "emacs-stupid-indent-mode")
    (version "20170525.1117")
    (source (origin
              (method url-fetch)
              (uri "https://melpa.org/packages/stupid-indent-mode-20170525.1117.el")
              (file-name (string-append name "-" version ".el"))
              (sha256
               (base32
                "11xbb8h7ap6kllfmpvv2hv0lq85ijy6wm4jqzshffydk367jv791"))))
    (build-system emacs-build-system)
    (home-page "https://melpa.org/packages/stupid-indent-mode-20170525.1117.el")
    (synopsis "Dumb indentation mode")
    (description "Dumb indentation mode is appropriate for editing buffers
that Emacs does not fully understand syntactically.")
    (license license:gpl3+)))

(define-public emacs-wi-utils
  (package
    (name "emacs-wi-utils")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kitnil/emacs-wi-utils")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d9i2l4wmpwvrkxbqwb546xb7j5ackfd129x0n3q9gspwd7745ys"))))
    (build-system emacs-build-system)
    (inputs
     `(("emacs-bui" ,emacs-bui)
       ("emacs-browse-at-remote" ,emacs-browse-at-remote)
       ("emacs-magit" ,emacs-magit)
       ("emacs-ivy" ,emacs-ivy)
       ("emacs-s" ,emacs-s)))
    (synopsis "Ivy procedures for Git project management")
    (description "This package provides several Emacs's Ivy procedures to work
with Git-based projects.")
    (home-page "https://github.com/kitnil/emacs-wi-utils")
    (license license:gpl3+)))

(define-public emacs-dorg
  (package
    (name "emacs-dorg")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kitnil/emacs-dorg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a3nzh5w5f1xz9bbw0pifqih1xgiy2zpn4gjd08iv0w086sr7q88"))))
    (build-system emacs-build-system)
    (inputs
     `(("emacs-debbugs" ,emacs-debbugs)
       ("emacs-org" ,emacs-org)
       ("emacs-el-mock" ,emacs-el-mock)))
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "-Q" "-batch" "-L" "."
                        "-l" "test/dorg-tests.el"
                        "-f" "ert-run-tests-batch-and-exit")))
    (home-page "https://github.com/kitnil/emacs-dorg")
    (synopsis "Manage Org-mode entries with Debbugs")
    (description "This package provides functions for Org-mode to manage
entries according to information received via Debbugs.")
    (license license:gpl3+)))

(define-public emacs-org-tanglesync-1.1.0
  (let ((commit "af83a73ae542d5cb3c9d433cbf2ce1d4f4259117")
        (revision "0"))
    (package
     (inherit emacs-org-tanglesync)
     (version (git-version "1.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mtekman/org-tanglesync.el")
             (commit commit)))
       (file-name (git-file-name (package-name emacs-org-tanglesync) version))
       (sha256
        (base32 "11rfn0byy0k0321w7fjgpa785ik1nrk1j6d0y4j0j4a8gys5hjr5"))))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
                       (add-after 'unpack 'patch
                                  (lambda _
                                    (substitute* "org-tanglesync.el"
                                                 (("\\(goto-char lmark\\)" line)
                                                  (string-append line " (org-end-of-line) (forward-char)")))))))))))

(define-public emacs-geiser-0.10
  (package
    (inherit emacs-geiser)
    (version "0.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/geiser/" version
                                 "/geiser-" version ".tar.gz"))
             (sha256
              (base32
               "0pj3l7p8d60c9b4vfprnv6g5l61d74pls4b5dvd84cn4ky9mzwjv"))))))

(define-public emacs-treemacs-no-tests
  (package
    (inherit emacs-treemacs)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-treemacs)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'check)))
       ((#:tests? tests?) #f)))))

(define-public emacs-lsp-java-fix-treemacs
  (package
    (inherit emacs-lsp-java)
    (propagated-inputs
     `(("emacs-treemacs" ,emacs-treemacs-no-tests)
       ,@(assoc-remove! (package-propagated-inputs emacs-lsp-java)
                        "emacs-treemacs")))))

(define-public emacs-slime-2.24
  (package
    (inherit emacs-slime)
    (name (package-name emacs-slime))
    (version "2.24")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slime/slime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0js24x42m7b5iymb4rxz501dff19vav5pywnzv50b673rbkaaqvh"))))))

(define-public emacs-tickscript-mode
  (package
    (name "emacs-tickscript-mode")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/msherry/tickscript-mode")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "173zk9nzjds0rkypmaq8xv5qianivgk16jpzgk0msdsn9kjbd8s9"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/msherry/tickscript-mode/")
    (synopsis "Major mode for Tickscript files")
    (description "Provides Emacs font-lock, indentation, navigation, and
utility functions for working with TICKscript, a DSL for use with Kapacitor
and InfluxDB.")
    (license license:gpl3+)))

(define-public emacs-org-timed-alerts
  (let ((commit "23cd318519edb035c5644726f29aea461b6401ca")
        (revision "1"))
    (package
      (name "emacs-org-timed-alerts")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/legalnonsense/org-timed-alerts")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03sf3rn3q1ivyb41fnaak01vn8bk6h2i8qycinf68lzxibaszj5k"))))
      (propagated-inputs
       `(("emacs-s", emacs-s)
         ("emacs-ts" ,emacs-ts)
         ("emacs-org-ql" ,emacs-org-ql)
         ("emacs-dash" ,emacs-dash)
         ("emacs-alert" ,emacs-alert)))
      (build-system emacs-build-system)
      (home-page "https://github.com/legalnonsense/org-timed-alerts/")
      (synopsis "Automatiic org timers for upcoming events")
      (description
       "This package provides automatiic org timers for upcoming events.")
      (license license:gpl3+))))

(define-public emacs-edit-as-format
  (package
    (name "emacs-edit-as-format")
    (version "20220221.1312")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etern/edit-as-format.git")
             (commit "59c6f439683846d994a7a2110b9b00cc16c08c40")))
       (sha256
        (base32 "0r2whzb3pizagbhr7i03kjiplnfwr1x14bl9y1gdvp166vfif5x7"))))
    (build-system emacs-build-system)
    (inputs (list pandoc))
    (propagated-inputs (list emacs-edit-indirect))
    (arguments
     `(#:include
       '("^[^/]+.el$"
         "^[^/]+.el.in$"
         "^dir$"
         "^[^/]+.info$"
         "^[^/]+.texi$"
         "^[^/]+.texinfo$"
         "^doc/dir$"
         "^doc/[^/]+.info$"
         "^doc/[^/]+.texi$"
         "^doc/[^/]+.texinfo$"
         "^filters$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-filters
           ;; This build phase installs ‘_simple-headers.lua’ file
           ;; and patches a location to this in ‘edit-as-format.el’.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "out") "/share/"
                                         ,name "-" ,version))
                    (filters (string-append data "/filters")))
               (install-file "filters/_simple-headers.lua" filters)
               (let ((file "edit-as-format.el"))
                 (chmod file #o644)
                 (emacs-substitute-sexps file
                   ("(defconst edit-as-format-filters-folder" filters)
                   ("(defcustom edit-as-format-pandoc-executable"
                    (which "pandoc"))))))))))
    (home-page "https://github.com/etern/edit-as-format")
    (synopsis "Edit document as other format")
    (description "Edit document as other format, using pandoc")
    (license license:gpl3+)))

(define-public emacs-stupid-indent-mode
  (package
    (name "emacs-stupid-indent-mode")
    (version "20170525.1117")
    (source
     (origin
       (method url-fetch)
       (uri "https://gist.githubusercontent.com/mishoo/5487564/raw/3295e7de5e2cfddc3bf0e462e852bf58972f5d70/stupid-indent-mode.el")
       (sha256
        (base32
         "1ik4ygjp4i2j6mph5mvmvjnnk1v90ilwxf7wv1wb1vccqfls6p92"))))
    (build-system emacs-build-system)
    (home-page "https://melpa.org/#/stupid-indent-mode")
    (synopsis "Plain stupid indentation minor mode")
    (description
     "Dumb indentation mode is appropriate for editing buffers that Emacs
does not fully understand syntactically")
    (license license:gpl3+)))

(define-public emacs-thesaurus
  (let ((commit "1d8ff1eabe6557341d266ae72a59ed9e27d28f2b"))
    (package
      (name "emacs-thesaurus")
      (version "0.0.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AnselmC/thesaurus.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1z250mlrfcfh25wndkjlc1ymq26y5spfwcfrmawf95yfab1wdamq"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-request" ,emacs-request)))
      (home-page "https://github.com/AnselmC/thesaurus.el")
      (synopsis "Query synonyms from Thesaurus.com")
      (description "Emacs package for querying synonyms from Thesaurus.com.")
      (license license:gpl3+))))

(define-public emacs-logstash
  (let ((commit "ebc4731c45709ad1e0526f4f4164020ae83cbeff")
        (revision "1"))
    (package
      (name "emacs-logstash")
      (version (git-version "0.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Wilfred/logstash-conf.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bjmd1xy45p4v2307sxd6mna9iqxvvz82sx5jbdf3hz5d71w5vfn"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/Wilfred/logstash-conf.el")
      (synopsis "Emacs major mode for editing logstash configuration files")
      (description "This package provides an Emacs major mode for editing logstash configuration
files.")
      (license license:gpl3+))))

(define-public emacs-yaml-pro
  (package
    (name "emacs-yaml-pro")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zkry/yaml-pro")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w2fpl3cidx4cy34wf8nx9ybwq6f69v4vc90zq7h70qmc9jm0phy"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-yaml))
    (home-page "https://github.com/zkry/yaml-pro/")
    (synopsis "Emacs library for editing yaml")
    (description "This package provides an Emacs library for editing yaml.

It utilizes the @code{emacs-yaml} parser to obtain a parse tree and is then
able to do things like move between subtrees, delete entire sections of
YAML (even if it's written in JSON style), and swap subtrees.")
    (license license:gpl3+)))

(define-public emacs-org-roam-ui
  (let ((commit "9fcc9a8d716254565d06082bc6e861b259c132fd")
        (revision "1"))
    (package
      (name "emacs-org-roam-ui")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/org-roam/org-roam-ui")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0mlvwgvvb6iwiwmmlfzi6gjy1ipcps800r0sxr25ldvz05n7av4w"))))
      (build-system emacs-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'install 'install-share
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((data (string-append (assoc-ref outputs "out")
                                          "/share/emacs-org-roam-ui")))
                 (chmod "org-roam-ui.el" #o644)
                 (emacs-substitute-sexps "org-roam-ui.el"
                   ("(defvar org-roam-ui-root-dir" data)
                   ("(defvar org-roam-ui-app-build-dir" data))
                 (mkdir-p data)
                 (copy-recursively "out" data)))))))
      (propagated-inputs
       `(("emacs-simple-httpd" ,emacs-simple-httpd)
         ("emacs-org-roam" ,emacs-org-roam)
         ("emacs-websocket" ,emacs-websocket)))
      (home-page "https://github.com/org-roam/org-roam-ui/")
      (synopsis "Rudimentary Roam replica with Org-mode")
      (description "Org-roam-ui provides a web interface for navigating around
notes created within Org-roam.")
      (license license:gpl3+))))
