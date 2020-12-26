;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services homer)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (homer-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the homer service.
;;;
;;; Code:

(define (homer-service-etc _)
  "Return a @file{/etc} entry for an @file{homer/config.yml}."
  `(("homer/config.yml"
     ,(computed-file
       "config.yml"
       #~(begin
           (copy-file #$(local-file "../../../homer/config.yml") #$output))))))

(define homer-service-type
  ;; The /etc/homer service.
  (service-type
   (name 'homer)
   (extensions
    (list (service-extension etc-service-type
                             homer-service-etc)))
   (default-value '())
   (description
    "Populate the @file{/etc/homer/config.yml} based on the given file object.")))

;;; homer.scm ends here
