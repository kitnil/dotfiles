;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services web)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (guix gexp)
  #:use-module (wigust packages web)
  #:use-module (gnu services configuration)
  #:use-module (json)
  #:export (homer-service-type
            homer-configuration
            %homer-nginx-configuration-nginx))

;;; Commentary:
;;;
;;; This module provides a service definition for the homer service.
;;;
;;; Code:


;;;
;;; Homer
;;;

(define %homer-nginx-configuration-nginx
  (nginx-server-configuration
   (root (file-append homer "/share/homer"))
   (listen '("80"))))

(define (file-object? val)
  (or (file-like? val) (file-name? val)))
(define (serialize-file-object field-name val)
  (serialize-string field-name val))

(define (nginx-server-configuration-list? val)
  (and (list? val) (and-map nginx-server-configuration? val)))
(define (serialize-nginx-server-configuration-list field-name val)
  "")

(define-configuration homer-configuration
  (config-file
   (file-object "homer.scm")
   "Path to a file containing configuration in JSON or YAML format.")
  (nginx
   (nginx-server-configuration-list
    (list %homer-nginx-configuration-nginx))
   "NGINX configuration."))

(define (homer-service-etc config)
  "Return a @file{/etc} entry for an @file{homer/config.yml}."
  `(("homer/config.yml" ,(homer-configuration-config-file config))))

(define homer-service-type
  ;; The /etc/homer service.
  (service-type
   (name 'homer)
   (extensions
    (list (service-extension etc-service-type
                             homer-service-etc)
          (service-extension nginx-service-type
                             homer-configuration-nginx)))
   (default-value '())
   (description
    "Populate the @file{/etc/homer/config.yml} based on the given file object.")))

;;; homer.scm ends here
