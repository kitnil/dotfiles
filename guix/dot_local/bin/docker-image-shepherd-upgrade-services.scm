;; upgrade shepherd services

;; oleg@workstation ~/src/dotfiles/guix$ sudo guix repl -L .
;; Consider installing the 'guile-readline' package for
;; convenient interactive line editing and input history.

;; Consider installing the 'guile-colorized' package
;; for a colorful Guile experience.

;; GNU Guile 3.0.9
;; Copyright (C) 1995-2023 Free Software Foundation, Inc.

;; Guile comes with ABSOLUTELY NO WARRANTY; for details type `,show w'.
;; This program is free software, and you are welcome to redistribute it
;; under certain conditions; type `,show c' for details.

;; Enter `,help' for help.
;; scheme@(guix-user)> (load "run.scm")

(use-modules ((gnu services) #:select (sexp->system-provenance))
             ((guix inferior) #:select (inferior-exception? inferior-exception-arguments))
             ((guix platform) #:select (systems))
             ((guix self) #:select (make-config.scm))
             (gcrypt pk-crypto)
             (gnu system file-systems)
             (gnu system uuid)
             (gnu system)
             (guix derivations)
             (guix diagnostics)
             (guix gexp)
             (guix i18n)
             (guix memoization)
             (guix modules)
             (guix monads)
             (guix pki)
             (guix records)
             (guix remote)
             (guix scripts system reconfigure)
             (guix ssh)
             (guix store)
             (guix utils)
             (ice-9 format)
             (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-26)
             (srfi srfi-34)
             (srfi srfi-35)
             (srfi srfi-9)
             (wugi system docker-image))

(run-with-store (open-connection)
  (mlet %store-monad ((a (upgrade-shepherd-services local-eval (%docker-image))))
    (return (list a))))
