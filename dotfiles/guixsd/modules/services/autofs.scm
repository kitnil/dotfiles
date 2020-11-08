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

(define-module (services autofs)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (gnu packages file-systems)
  #:use-module (ice-9 match)
  #:export (autofs-configuration
            autofs-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the SERVICE_NAME
;;; SERVICE_DESCRTIPTION
;;;
;;; Code:

(define-record-type* <autofs-configuration>
  autofs-configuration make-autofs-configuration
  autofs-configuration?
  (autofs autofs-configuration-autofs
          (default autofs))
  (pid-file autofs-configuration-pid-file
            (default "/var/run/autofs"))
  (config-file autofs-configuration-config-file
               (default (plain-file "empty" ""))))

(define (autofs-activation config)
  "Return the activation gexp for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/autofs")))

(define autofs-shepherd-service
  ;; Return a <shepherd-service> running autofs.
  (match-lambda
    (($ <autofs-configuration> autofs pid-file config-file)
     (list (shepherd-service
            (provision '(autofs))
            (documentation "Run autofs daemon.")
            (requirement '(user-processes loopback))
            (start #~(make-forkexec-constructor
                      (list (string-append #$autofs "/sbin/automount")
                            "-f" "-p" #$pid-file #$config-file)
                      #:pid-file #$pid-file)))))))

(define autofs-service-type
  (service-type
   (name 'autofs)
   (extensions
    (list (service-extension activation-service-type
                             autofs-activation)
          (service-extension shepherd-root-service-type
                             autofs-shepherd-service)))
   (default-value (autofs-configuration))
   (description
    "Run the autofs.")))

;;; autofs.scm ends here
