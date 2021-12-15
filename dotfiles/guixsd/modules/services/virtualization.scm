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

(define-module (services virtualization)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages docker)
  #:export (amdgpu-passthrough-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the docker service.
;;;
;;; Code:

(define amdgpu-passthrough-start
  (program-file
   "amdgpu-passthrough-set-reset-method"
   #~(begin
       (use-modules (ice-9 rdelim)
                    (srfi srfi-34))
       (let loop ()
         (if (string= (string-trim-right (with-input-from-file "/sys/bus/pci/devices/0000:12:00.0/reset_method" read-string)) "device_specific")
             #t
             (guard (c (#t (begin (display "amdgpu-passthrough-set-reset-method failed to set device_specific reset_method\n") #f)))
               (with-output-to-file "/sys/bus/pci/devices/0000:12:00.0/reset_method"
                 (lambda ()
                   (display "device_specific\n")))))
         (sleep 5)
         (loop)))))

(define amdgpu-passthrough-service
  (simple-service 'amdgpu-passthrough shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(amdgpu-passthrough-start))
                    (documentation "Enable device_specific reset for amdgpu-passthrough.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list #$amdgpu-passthrough-start)))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))

;;; docker.scm ends here
