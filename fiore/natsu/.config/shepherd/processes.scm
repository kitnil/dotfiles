;;; processes.scm --- Utilities for working with processes

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 19 Mar 2016

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides code related to processes.

;;; Code:

(define-module (dotfiles guile processes)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:export (environment-excursion
            with-environment-excursion
            system*-no-output
            process-exists?))

(define (environment-excursion env-thunk body-thunk)
  "Run BODY-THUNK with the current environment set by ENV-THUNK."
  (let ((old-env (environ)))
    (dynamic-wind
      env-thunk
      body-thunk
      (lambda () (environ old-env)))))

(define-syntax-rule (with-environment-excursion env body ...)
  "Run BODY with ENV as the process's current environment."
  (environment-excursion
   (lambda () (environ env))
   (lambda () body ...)))

(define* (process-exists? regexp #:key uid exact?)
  "Return #t if process defined by REGEXP exists.
If UID is specified, only check processes with this real user ID.
If EXACT? is #t, check processes which exactly match REGEXP."
  (let ((args `("pgrep" ,regexp
                ,@(if exact? '("--exact") '())
                ,@(if uid
                      (list "--uid" (number->string uid))
                      '()))))
    (zero? (status:exit-val (apply system* args)))))

(define (system*-no-output . args)
  "Like 'system*' but suppress the output of the command indicated by ARGS."
  (let ((port (apply open-pipe* OPEN_READ args)))
    (read-string port)
    (close-pipe port)))

;;; processes.scm ends here
