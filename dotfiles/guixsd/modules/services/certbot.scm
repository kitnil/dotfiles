;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services certbot)
  #:use-module (gnu services certbot)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (ice-9 match)
  #:export (certbot-service-type-custom-nginx))

;;; Commentary:
;;;
;;; This module provides a service definition for the certbot service.
;;;
;;; Code:

(define (certbot-custom-nginx-server-configurations listen)
  (match-lambda
    (($ (@@ (gnu services certbot) <certbot-configuration>)
        package webroot certificates email
        server rsa-key-size default-location)
     (list
      (nginx-server-configuration
       (listen (list (string-append listen ":80")))
       (ssl-certificate #f)
       (ssl-certificate-key #f)
       (server-name
        (apply append
               (map (@@ (gnu services certbot) certificate-configuration-domains)
                    certificates)))
       (locations
        (filter identity
                (list
                 (nginx-location-configuration
                  (uri "/.well-known")
                  (body (list (list "root " webroot ";"))))
                 default-location))))))))

(define (certbot-service-type-custom-nginx listen)
  (service-type
   (inherit certbot-service-type)
   (extensions (append (filter (lambda (extension)
                                 (not (equal? (service-extension-target extension)
                                              nginx-service-type)))
                               (service-type-extensions certbot-service-type))
                       (list (service-extension nginx-service-type
                                                (certbot-custom-nginx-server-configurations listen)))))))

;;; certbot.scm ends here
