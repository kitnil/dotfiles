#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (jenkins\ scripts\ plugins) -s
!#

;;;; plugins --- Convert jenkinsPlugins2nix output to Scheme
;;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;;; Released under the GNU GPLv3 or any later version.

(define-module (jenkins scripts plugins)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-69)
  #:export (main))

;;; Commentary:
;;;
;;; This Guile script converts
;;; <https://github.com/Fuuzetsu/jenkinsPlugins2nix/> Nix expression to Guix
;;; package recipes.  To do that, run:
;;;
;;;  ./plugins.scm plugins.nix
;;;
;;; Code:

(define %options
  (let ((display-and-exit-proc (lambda (msg)
                                 (lambda (opt name arg loads)
                                   (display msg) (quit)))))
    (list (option '(#\v "version") #f #f
                  (display-and-exit-proc "plugins version 0.0.1\n"))
          (option '(#\h "help") #f #f
                  (display-and-exit-proc
                   "Usage: plugins ...")))))

(define %default-options
  '())

(define (nix-expression filename)
  (format #f "\
with import <nixpkgs> { };
with lib;
mapAttrsFlatten (name: value: { \"source\" = head value.src.urls; })
(import ~a {
  inherit fetchurl stdenv;
})
" filename))

(define (main args)
  (define opts
    (args-fold (cdr (program-arguments))
               %options
               (lambda (opt name arg loads)
                 (error "Unrecognized option `~A'" name))
               (lambda (op loads)
                 (cons op loads))
               %default-options))
  (let* ((port (open-pipe* OPEN_READ "nix-instantiate" "--json" "--strict"
                           "--eval" "--expr" (nix-expression (last args))))
         (output (read-string port)))
    (pretty-print '(use-modules (guix)
                                (guix download)))
    (newline)
    (pretty-print
     `(list
       ,@(map (lambda (input)
                `(origin
                   (method url-fetch)
                   (uri ,(assoc-ref input "source"))
                   (sha256 #f)))
              (vector->list (json-string->scm output)))))
    (close-port port)))

;;; plugins ends here
