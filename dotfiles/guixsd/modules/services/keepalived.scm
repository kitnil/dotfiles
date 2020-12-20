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

(define-module (services keepalived)
  #:use-module (gnu packages cluster)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (keepalived-configuration
            keepalived-configuration?
            keepalived-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the keepalived service.
;;;
;;; Code:

(define-record-type* <keepalived-configuration>
  keepalived-configuration make-keepalived-configuration
  keepalived-configuration?
  (keepalived  keepalived-configuration-keepalived  ;<package>
               (default keepalived))
  (config-file keepalived-configuration-config-file ;file-like
               (default #f)))

(define keepalived-shepherd-service
  (match-lambda
    (($ <keepalived-configuration> keepalived config-file)
     (list
      (shepherd-service
       (provision '(keepalived))
       (documentation "Run keepalived.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list (string-append #$keepalived "/sbin/keepalived")
                       "--dont-fork" "--log-console" "--log-detail"
                       "--pid=/var/run/keepalived.pid"
                       (string-append "--use-file=" #$config-file))
                 #:pid-file "/var/run/keepalived.pid"
                 #:log-file "/var/log/keepalived.log"))
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define keepalived-service-type
  (service-type (name 'keepalived)
                (extensions (list (service-extension shepherd-root-service-type
                                                     keepalived-shepherd-service)))
                (description "Run keepalived.")))

;;; keepalived.scm ends here
