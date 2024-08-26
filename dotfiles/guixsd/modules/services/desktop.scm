;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services desktop)
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (seatd-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the desktop service.
;;;
;;; Code:

(define seatd-service
  (simple-service 'seatd shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(seatd))
                    (documentation "Run seatd.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list #$(file-append seatd "/bin/seatd"))
                              "-u" "oleg"
                              "-g" "users"))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))

;;; desktop.scm ends here
