;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
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


;; This file contains machinery to find all my packages.  To do that, run:
;;
;;  guile -l src/guix/etc/mypackages.scm -c exit
;;
;; The result is a directory hierarchy that can be used as the manual/
;; sub-directory of the web site.

(use-modules (gnu packages)
             (guix build utils)
             (guix channels)
             (guix git)
             (guix packages)
             (guix profiles)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26))

(define %file-dir (dirname (current-filename)))

(define %guix-git "https://git.savannah.gnu.org/git/guix.git")

(define %source-dir
  ((@@ (guix git) url-cache-directory) %guix-git))

(define %author "go.wigust@gmail.com")

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-port port)
      (string-trim-right output #\newline))))

(define (string-drop-suffix suffix string)
  (if (string-suffix? suffix string)
      (string-drop-right string (string-length suffix))
      string))

(define (string-drop-prefix prefix string)
  (if (string-prefix? prefix string)
      (string-drop string (string-length prefix))
      string))

(update-cached-checkout
 %guix-git
 #:ref `(commit . ,(channel-commit (first (filter (lambda (channel)
                                                    (string=? (symbol->string (channel-name channel))
                                                              "guix"))
                                                  (load (string-append (dirname (dirname %file-dir)) "/channels.scm")))))))

(let ((packages (fold append '()
                      (map (compose (cut find-packages-by-name <>)
                                    (cut string-drop-prefix "gnu: Add " <>)
                                    (cut string-drop-suffix "." <>))
                           (filter (cut string-prefix? "gnu: Add" <>)
                                   (string-split (git-output "log" (string-append "--author=" %author) "--format=%s")
                                                 #\newline))))))
  (packages->manifest packages))
