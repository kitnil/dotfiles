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

(define-module (services dns)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:export (knot-dns-configuration
            knot-dns-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the dns service.
;;;
;;; Code:

(define-record-type* <knot-dns-configuration>
  knot-dns-configuration make-knot-dns-configuration
  knot-dns-configuration?
  (knot-dns knot-dns-configuration-knot-dns ;<package>
            (default knot))
  (config-file knot-dns-configuration-config-file ;<file-like>
               (default #f))
  (run-directory knot-dns-configuration-run-directory
                 (default "/var/run/knot")))

(define %knot-dns-accounts
  (list (user-group (name "knot") (system? #t))
        (user-account
          (name "knot")
          (group "knot")
          (system? #t)
          (comment "knot dns server user")
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define (knot-dns-activation config)
  (with-imported-modules (source-module-closure '((gnu build activation)))
    #~(begin
        (use-modules (gnu build activation))
        (mkdir-p/perms #$(knot-dns-configuration-run-directory config)
                       (getpwnam "knot") #o755)
        (mkdir-p/perms "/var/lib/knot" (getpwnam "knot") #o755)
        (mkdir-p/perms "/var/lib/knot/keys" (getpwnam "knot") #o755)
        (mkdir-p/perms "/var/lib/knot/keys/keys" (getpwnam "knot") #o755))))

(define (knot-dns-shepherd-service config)
  (let* ((config-file (knot-dns-configuration-config-file config))
         (knot-dns (knot-dns-configuration-knot-dns config)))
    (list (shepherd-service
            (documentation "Run the Knot DNS daemon.")
            (provision '(knot dns))
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                       (list (string-append #$knot "/sbin/knotd")
                             "-c" #$config-file)))
            (stop #~(make-kill-destructor))))))

(define knot-dns-service-type
  (service-type (name 'knot)
                (extensions
                  (list (service-extension shepherd-root-service-type
                                           knot-dns-shepherd-service)
                        (service-extension activation-service-type
                                           knot-dns-activation)
                        (service-extension account-service-type
                                           (const %knot-dns-accounts))))
                (description
                 "Run @uref{https://www.knot-dns.cz/, Knot}, an authoritative
name server for the @acronym{DNS, Domain Name System}.")))

;;; dns.scm ends here
