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

(define-module (wigust services autossh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (autossh-configuration
            autossh-service-type
            autossh-client-configuration
            autossh-client-host-configuration

            autossh-client-configuration-fields
            autossh-client-host-configuration-fields
            serialize-autossh-client-host-configuration-list))

;;; Commentary:
;;;
;;; This module provides a service definition for the autossh
;;; which could be used to establish a SSH proxy connection.
;;;
;;; Code:

(define (uglify-field-name field-name)
  (apply string-append
         (map string-capitalize
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define (serialize-field field-name val)
  #~(format #f "~a ~a~%" #$(uglify-field-name field-name) #$val))

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val))))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define (serialize-boolean field-name val)
  (serialize-string field-name (if val "yes" "no")))

(define extra-options? string?)
(define (serialize-extra-options field-name val)
  #~(format #f "~a~%" #$val))

(define-configuration autossh-client-host-configuration
  (host
   (string "")
   "")
  (host-name
   (string "")
   "")
  (port
   (number 22)
   "")
  (user
   (string "")
   "")
  (strict-host-key-checking?
   (boolean #t)
   "")
  (identity-file
   (string "")
   "")
  (user-known-hosts-file
   (string "")
   "")
  (extra-options
   (extra-options "")
   ""))

(define (serialize-autossh-client-host-configuration-list field-name val)
  #~(string-append
     #$@(map (cut serialize-configuration <>
                  autossh-client-host-configuration-fields)
             val)))

(define (autossh-client-host-configuration-list? val)
  (list? val))

(define-configuration autossh-client-configuration
  (hosts
   (autossh-client-host-configuration-list '())
   "List of hosts."))

(define-record-type* <autossh-configuration>
  autossh-configuration make-autossh-configuration
  autossh-configuration?
  (user autossh-configuration-user ;string
        (default "autossh"))
  (group autossh-configuration-group ;string
         (default "autossh"))
  (user-id autossh-configuration-user-id ;number
           (default #f))
  (group-id autossh-configuration-group-id ;number
            (default #f))
  (autossh-client-config autossh-configuration-autossh-client-config
                         (default #f))
  (host autossh-configuration-host ;string
        (default "")))

(define autossh-account
  ;; Return the user accounts and user groups for CONFIG.
  (match-lambda
    (($ <autossh-configuration> user-name group-name user-id group-id _ _)
     (list (user-group
            (name group-name)
            (id group-id))
           (user-account
            (name user-name)
            (uid user-id)
            (group group-name)
            (comment "autossh privilege separation user")
            (home-directory (string-append "/var/run/" user-name))
            (shell #~(string-append #$shadow "/sbin/nologin")))))))

(define autossh-activation
  (match-lambda
    (($ <autossh-configuration> user-name group-name user-id group-id _ _)
     (with-imported-modules '((guix build utils))
       #~(begin
           (let* ((user  (getpw (if #$user-id #$user-id #$user-name)))
                  (group (getpw (if #$group-id #$group-id #$group-name)))
                  (chown* (lambda (file)
                            (chown file (passwd:uid user) (group:gid group)))))
             (mkdir-p "/etc/autossh")
             (chown* "/etc/autossh")
             (for-each chown* (find-files "/etc/autossh"))))))))

(define autossh-shepherd-service
  (match-lambda
    (($ <autossh-configuration> user-name group-name _ _
                                autossh-client-config host)
     (list (shepherd-service
            (provision '(autossh))
            (documentation "Run autossh daemon.")
            (start #~(make-forkexec-constructor
                      (list (string-append #$autossh "/bin/autossh")
                            "-NT" "-M" "0"
                            "-F" #$(mixed-text-file "ssh-client.conf"
                                                    (serialize-configuration autossh-client-config
                                                                             autossh-client-configuration-fields))
                            #$host)
                      #:user #$user-name
                      #:group #$group-name
                      #:environment-variables
                      (list "AUTOSSH_PORT=0"
                            "AUTOSSH_GATETIME=0")))
            (stop #~(make-kill-destructor)))))))

(define autossh-service-type
  (service-type
   (name 'autossh)
   (extensions
    (list (service-extension shepherd-root-service-type
                             autossh-shepherd-service)
          (service-extension account-service-type
                             autossh-account)
          (service-extension activation-service-type
                             autossh-activation)))
   (default-value (autossh-configuration))
   (description
    "Run the autossh.")))

;;; autossh.scm ends here
