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

(define-module (services mail)
  #:use-module ((gnu services mail) #:select (mail-aliases-service-type))
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (exim-configuration
            exim-configuration?
            exim-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the mail service.
;;;
;;; Code:

;;;
;;; Exim.
;;;

(define-record-type* <exim-configuration> exim-configuration
  make-exim-configuration
  exim-configuration?
  (package       exim-configuration-package ;<package>
                 (default exim))
  (config-file   exim-configuration-config-file ;file-like
                 (default #f)))

(define %exim-accounts
  (list (user-group
         (name "exim")
         (system? #t))
        (user-account
         (name "exim")
         (group "exim")
         (system? #t)
         (comment "Exim Daemon")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (exim-computed-config-file package config-file)
  (computed-file "exim.conf"
                 #~(call-with-output-file #$output
                     (lambda (port)
                       (format port "
exim_user = exim
exim_group = exim
.include ~a"
                               #$(or config-file
                                     (file-append package "/etc/exim.conf")))))))

(define exim-shepherd-service
  (match-lambda
    (($ <exim-configuration> package config-file)
     (list (shepherd-service
            (provision '(exim mta))
            (documentation "Run the exim daemon.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      '(#$(file-append package "/bin/exim")
                        "-bd" "-v" "-C"
                        #$(exim-computed-config-file package config-file))))
            (stop #~(make-kill-destructor)))))))

(define exim-activation
  (match-lambda
    (($ <exim-configuration> package config-file)
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))

           (let ((uid (passwd:uid (getpw "exim")))
                 (gid (group:gid (getgr "exim"))))
             (mkdir-p "/var/spool/exim")
             (chown "/var/spool/exim" uid gid))

           (zero? (system* #$(file-append package "/bin/exim")
                           "-bV" "-C" #$(exim-computed-config-file package config-file))))))))

(define exim-profile
  (compose list exim-configuration-package))

(define exim-service-type
  (service-type
   (name 'exim)
   (extensions
    (list (service-extension shepherd-root-service-type exim-shepherd-service)
          (service-extension account-service-type (const %exim-accounts))
          (service-extension activation-service-type exim-activation)
          (service-extension profile-service-type exim-profile)
          (service-extension mail-aliases-service-type (const '()))))))

;;; mail.scm ends here
