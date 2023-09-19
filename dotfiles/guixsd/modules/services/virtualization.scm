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

(define-module (services virtualization)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (vm-win10-service-type
            vm-win2022-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the virtualization service.
;;;
;;; Code:

(define %vm-win2022-log
  "/var/log/vm-win2022.log")

(define (vm-win2022-log-rotations config)
  (list (log-rotation
         (files (list %vm-win2022-log)))))

(define (vm-win2022-shepherd-service config)
  (list
   (shepherd-service
    (provision '(vm-win2022))
    (documentation "Run Windows 2022 virtual machine.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list "/home/oleg/.local/share/chezmoi/dot_local/bin/executable_virsh"
                    "start" "win2022")
              #:environment-variables '("VIRSH_DAEMON=true")
              #:log-file #$%vm-win2022-log))
    (respawn? #t) ;XXX: Fix race condition with Docker
    (stop #~(make-kill-destructor)))))

(define vm-win2022-service-type
  (service-type
   (name 'vm-win2022)
   (extensions
    (list (service-extension shepherd-root-service-type
                             vm-win2022-shepherd-service)
          (service-extension rottlog-service-type
                             vm-win2022-log-rotations)))
   (default-value '())
   (description "Run the vm-win2022.")))


;;;
;;; Windows 10
;;;

(define %vm-win10-log
  "/var/log/vm-win10.log")

(define (vm-win10-log-rotations config)
  (list (log-rotation
         (files (list %vm-win10-log)))))

(define (vm-win10-shepherd-service config)
  (list
   (shepherd-service
    (provision '(vm-win10))
    (documentation "Run Windows 10 virtual machine.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list #$(local-file "/home/oleg/.local/share/chezmoi/dot_local/bin/executable_virsh")
                    "start" "win10")
              ;; #:pid-file "/var/run/libvirt/qemu/win10.pid"
              #:environment-variables '("VIRSH_DAEMON=true")
              #:log-file #$%vm-win10-log))
    (respawn? #f)
    (stop #~(lambda _
              (begin
                (define (wait-for-missing-file file)
                  ;; Wait until FILE shows up.
                  (let loop ((i 120))
                    (cond ((not (file-exists? file))
                           #t)
                          ((zero? i)
                           (error "file still exists" file))
                          (else
                           (pk 'wait-for-missing-file file)
                           (sleep 1)
                           (loop (- i 1))))))
                (invoke #$(file-append libvirt "/bin/virsh")
                        "shutdown" "win10")
                (wait-for-missing-file "/var/run/libvirt/qemu/win10.pid")
                (invoke "/home/oleg/.local/share/chezmoi/dot_local/bin/executable_gpu_reset.sh")))))))

(define vm-win10-service-type
  (service-type
   (name 'vm-win10)
   (extensions
    (list (service-extension shepherd-root-service-type
                             vm-win10-shepherd-service)
          (service-extension rottlog-service-type
                             vm-win10-log-rotations)))
   (default-value '())
   (description "Run the vm-win10.")))

;;; virtualization.scm ends here
