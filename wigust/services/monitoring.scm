;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust services monitoring)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (wigust packages monitoring)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (zabbix-service-type))

(define (uglify-field-name field-name)
  (string-delete #\? (symbol->string field-name)))

(define (serialize-field field-name val)
  (format #t "~a=~a\n" (uglify-field-name field-name) val))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

;; (define-configuration zabbix-configuration
;;   (state-directory
;;    (string "/var/run/zabbix")
;;    "")
;;   (log-directory
;;    (string "/var/log/zabbix")
;;    "")
;;   (library-directory
;;    (string "/var/lib/zabbix")
;;    "")
;;   (pid-file
;;    (string "/var/run/zabbix/zabbix_server.pid")))

(define (zabbix-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user "zabbix")
        (zabbix-group "zabbix"))
      (list (user-group (name zabbix-group) (system? #t))
        (user-account
         (name zabbix-user)
         (system? #t)
         (group zabbix-group)
         (comment "zabbix privilege separation user")
         (home-directory (string-append "/var/run/" zabbix-user))
         (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define zabbix-service-type
  (service-type
   (name 'zabbix)
   (extensions
    (list (service-extension account-service-type zabbix-account)))
   (default-value '())))
