;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services backup)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (restic-rest-configuration
            restic-rest-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the backup service.
;;;
;;; Code:

(define-record-type* <restic-rest-configuration>
  restic-rest-configuration make-restic-rest-configuration
  restic-rest-configuration?
  (restic-rest     restic-rest-configuration-restic-rest     ;string
                   (default #f))
  (listen-address  restic-rest-configuration-listen-address  ;string
                   (default "0.0.0.0:8080"))
  (data-path       restic-rest-configuration-data-path       ;string
                   (default "/var/lib/restic"))
  (append-only?    restic-rest-configuration-append-only?    ;boolean
                   (default #f))
  (prometheus?     restic-rest-configuration-prometheus?     ;boolean
                   (default #f))
  (private-repos?  restic-rest-configuration-private-repos?  ;boolean
                   (default #f))
  (arguments       restic-rest-configuration-arguments       ;list of strings
                   (default '()))
  (user            restic-rest-configuration-user            ;string
                   (default "restic"))
  (group           restic-rest-configuration-group           ;string
                   (default "restic"))
  (authentication? restic-rest-configuration-authentication? ; boolean
                   (default #t)))

(define (restic-account config)
  (list (user-account
         (name "restic")
         (group "restic")
         (system? #t)
         (comment "Restic REST privilege separation user")
         (home-directory "/var/lib/restic"))
        (user-group
         (name "restic")
         (system? #t))))

(define (restic-rest-activation config)
  #~(begin
      (let* ((user (getpw "restic"))
             (home (passwd:dir user))
             (uid (passwd:uid user))
             (group (getgrnam "restic"))
             (gid (group:gid group))
             (data-path #$(restic-rest-configuration-data-path config)))
        (mkdir-p data-path)
        (chown data-path uid gid))))

(define (restic-rest-shepherd-service config)
  (list
   (shepherd-service
    (provision '(restic-rest))
    (documentation "Run Restic REST.")
    (requirement '(user-processes loopback))
    (start #~(make-forkexec-constructor
              (list #$(restic-rest-configuration-restic-rest config)
                    "--listen" #$(restic-rest-configuration-listen-address config)
                    "--path" #$(restic-rest-configuration-data-path config)
                    #$@(if (restic-rest-configuration-append-only? config)
                           '("--append-only")
                           '())
                    #$@(if (restic-rest-configuration-private-repos? config)
                           '("--private-repos")
                           '())
                    #$@(if (restic-rest-configuration-prometheus? config)
                           '("--prometheus")
                           '())
                    #$@(if (restic-rest-configuration-authentication? config)
                           '()
                           '("--no-auth"))
                    #$@(restic-rest-configuration-arguments config))
              #:log-file "/var/log/restic-rest.log"
              #:user #$(restic-rest-configuration-user config)
              #:group #$(restic-rest-configuration-group config)))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define restic-rest-service-type
  (service-type (name 'restic-rest)
                (extensions (list (service-extension account-service-type
                                                     restic-account)
                                  (service-extension shepherd-root-service-type
                                                     restic-rest-shepherd-service)
                                  (service-extension activation-service-type
                                                     restic-rest-activation)))
                (description "Run Restic REST.")))

;;; backup.scm ends here
