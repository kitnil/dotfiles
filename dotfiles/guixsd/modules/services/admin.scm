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

(define-module (services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (packages admin)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (osquery-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the admin service.
;;;
;;; Code:

(define (osquery-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (mkdir-p "/var/osquery")
        (mkdir-p "/var/log/osquery"))))

(define (osquery-shepherd-service config)
  (list
   (shepherd-service
    (provision '(osquery))
    (documentation "Run osquery.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list #$(file-append osquery "/bin/osqueryd"))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define osquery-service-type
  (service-type
   (name 'osquery)
   (extensions
    (list (service-extension profile-service-type
                             (lambda (config)
                               (list osquery)))
          (service-extension shepherd-root-service-type
                             osquery-shepherd-service)
          (service-extension activation-service-type
                             osquery-activation)))
   (default-value '())
   (description "Run the osquery.")))

;;; admin.scm ends here
