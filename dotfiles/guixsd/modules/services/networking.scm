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

(define-module (services networking)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (gre-configuration
            gre-configuration?
            gre-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the networking service.
;;;
;;; Code:

(define-record-type* <gre-configuration>
  gre-configuration make-gre-configuration
  gre-configuration?
  (iproute           gre-configuration-iproute           ;<package>
                     (default iproute))
  (ip-address-local  gre-configuration-ip-address-local  ;string
                     (default #f))
  (ip-address-remote gre-configuration-ip-address-remote ;string
                     (default #f))
  (ip-address        gre-configuration-ip-address        ;string
                     (default #f))
  (interface-name    gre-configuration-interface-name    ;string
                     (default #f))
  (routes            gre-configuration-routes            ;list of strings
                     (default #f)))

(define gre-service-stop ;; is not used in one-shot type
  (program-file "gre-stop"
                #~(begin
                    (let ((ip (string-append #$iproute "/sbin/ip")))
                      (display "Deleting GRE tunnel...\n")))))

(define gre-shepherd-service
  (match-lambda
    (($ <gre-configuration> iproute ip-address-local ip-address-remote ip-address interface-name routes)
     (list
      (shepherd-service
       (provision '(gre))
       (documentation "Run GRE tunnel.")
       (requirement '(loopback))
       (start #~(make-forkexec-constructor
                 (list #$(program-file "gre-start"
                                       #~(begin
                                           (let ((ip (string-append #$iproute "/sbin/ip")))
                                             (display "Creating GRE tunnel...\n")
                                             (apply system* (list ip "tunnel" "add" #$interface-name "mode" "gre" "local" #$ip-address-local "remote" #$ip-address-remote "ttl" "255"))
                                             (apply system* (list ip "addr" "add" #$ip-address "dev" #$interface-name))
                                             (apply system* (list ip "link" "set" #$interface-name "up"))
                                             (for-each (lambda (route)
                                                         (apply system* (append (list ip "route")
                                                                                (string-split route #\space))))
                                                       '#$routes)))))))
       (respawn? #f)
       (one-shot? #t)
       (stop #~(make-forkexec-constructor
                (list #$gre-service-stop))))))))

(define gre-service-type
  (service-type (name 'gre)
                (extensions (list (service-extension shepherd-root-service-type
                                                     gre-shepherd-service)))
                (description "Run GRE tunnel.")
                (default-value '())))

;;; networking.scm ends here
