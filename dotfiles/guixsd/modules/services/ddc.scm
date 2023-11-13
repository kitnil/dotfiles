;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services ddc)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (ddcutil-daemon-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the ddc service.
;;;
;;; Code:

(define ddcutil-daemon-service
  (simple-service 'ddcutil-daemon shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(ddcutil-daemon))
                    (documentation "Run ddcutil-daemon.")
                    (requirement '(user-processes loopback))
                    (start #~(make-forkexec-constructor
                              (list #$(local-file "/home/oleg/src/gitlab.com/wigust/ddcutil-daemon/run.sh"
                                                  #:recursive? #t))))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))

;;; ddc.scm ends here
