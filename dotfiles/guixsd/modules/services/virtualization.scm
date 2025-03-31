;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2025 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (virtual-machine
            virtual-machine-service-type

            runc-configuration
            runc-configuration?
            runc-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the virtualization service.
;;;
;;; Code:

(define-record-type* <virtual-machine>
  virtual-machine make-virtual-machine
  virtual-machine?
  (name        virtual-machine-name)       ;string
  (auto-start? virtual-machine-auto-start? ;boolean
               (default #f)))

(define (virtual-machine-log-rotations config)
  (list
   (log-rotation
    (files
     (list
      (string-append "/var/log/virtual-machine-" (virtual-machine-name config) ".log"))))))

(define (virtual-machine-shepherd-service config)
  (list
   (shepherd-service
    (provision
     (list
      (string->symbol
       (string-append "virtual-machine-" (virtual-machine-name config)))))
    (documentation "Run virtual machine.")
    (requirement '())
    (start
     #~(make-forkexec-constructor
        (list #$(local-file "/home/oleg/.local/share/chezmoi/dot_local/bin/executable_virsh"
                            #:recursive? #t)
              "start" #$(virtual-machine-name config))
        #:environment-variables '("VIRSH_DAEMON=true")
        #:log-file
        #$(string-append "/var/log/virtual-machine-" (virtual-machine-name config) ".log")))
    (respawn? #f)
    (auto-start? #f)
    (stop #~(lambda _
              (begin
                (use-modules (ice-9 format))
                (define (wait-for-missing-file file)
                  ;; Wait until FILE disappears.
                  (let loop ((i 120))
                    (cond ((not (file-exists? file))
                           #t)
                          ((zero? i)
                           (error "file still exists" file))
                          (else
                           (pk 'wait-for-missing-file
                               (format #f "[~a/120]: ~a~%" i file))
                           (sleep 1)
                           (loop (- i 1))))))
                (invoke #$(file-append libvirt "/bin/virsh")
                        "shutdown" #$(virtual-machine-name config))
                (wait-for-missing-file
                 #$(string-append "/var/run/libvirt/qemu/"
                                  (virtual-machine-name config) ".pid"))
                (invoke
                 #$(local-file "/home/oleg/.local/share/chezmoi/dot_local/bin/executable_gpu_reset.sh"
                               #:recursive? #t))))))))

(define virtual-machine-service-type
  (service-type
   (name 'virtual-machine)
   (extensions
    (list (service-extension shepherd-root-service-type
                             virtual-machine-shepherd-service)
          (service-extension rottlog-service-type
                             virtual-machine-log-rotations)))
   (default-value '())
   (description "Run virtual machine.")))


;;;
;;; runc
;;;

(define-record-type* <runc-configuration>
  runc-configuration make-runc-configuration
  runc-configuration?
  (runc runc-configuration-runc ;<package>
        (default runc))
  (name runc-configuration-name) ;string
  (directory runc-configuration-directory)) ;string

(define (runc-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (mkdir-p "/var/log/runc"))))

(define (runc-log-rotations config)
  (list
   (log-rotation
    (files
     (list
      (string-append "/var/log/runc/"
                     (runc-configuration-name config) ".log"))))))

(define (runc-shepherd-service config)
  (match-record
   config <runc-configuration>
   (runc name directory)
   (list
    (shepherd-service
     (provision (list (string->symbol (string-append "runc-" name))))
     (documentation "Run container with runc.")
     (requirement '(networking))
     (start #~(make-forkexec-constructor
               (list (string-append #$runc "/bin/runc")
                     "run" name)
               #:directory #$directory
               #:log-file #$(string-append "/var/log/runc/" name ".log")))
     (respawn? #f)
     (auto-start? #f)
     (stop #~(make-kill-destructor))))))

(define runc-service-type
  (service-type (name 'runc)
                (extensions (list (service-extension activation-service-type
                                                     runc-activation)
                                  (service-extension shepherd-root-service-type
                                                     runc-shepherd-service)
                                  (service-extension rottlog-service-type
                                                     runc-log-rotations)))
                (description "Run runc.")))

;;; virtualization.scm ends here
