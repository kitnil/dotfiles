;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
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
            autofs-configuration?
            autofs-service-type

            autofs-mount-configuration
            autofs-mount-configuration?

            ;; XXX: Don't need to export autofs-configuration-file
            autofs-configuration-file))

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
               (default (plain-file "empty" "")))
  (mounts autofs-configuration-mounts ;list of <autofs-mount-configuration>
          (default '())))

(define-record-type* <autofs-mount-configuration>
  autofs-mount-configuration make-autofs-mount-configuration
  autofs-mount-configuration?
  (target autofs-mount-configuration-target ;string
          (default #f))
  (source autofs-mount-configuration-source ;string
          (default #f))
  (fstype autofs-mount-configuration-fstype ;string
          (default "-fstype=fuse,rw,allow_other")))

(define (autofs-configuration-file config)
  (define autofs-mounts-configuration-file
    (plain-file "autofs-mounts.conf"
                (call-with-output-string
                  (lambda (port)
                    (match-record config <autofs-configuration>
                      (autofs pid-file config-file mounts)
                      (for-each (lambda (mount)
                                  (match-record mount <autofs-mount-configuration>
                                    (target source fstype)
                                    (display (string-join (list target fstype source))
                                             port)
                                    (newline port)))
                                mounts))))))
  (mixed-text-file
   "autofs.conf"
   "/- " autofs-mounts-configuration-file " --timeout=5"))

(define (autofs-activation config)
  "Return the activation gexp for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/autofs")))

(define (autofs-shepherd-service config)
  ;; Return a <shepherd-service> running autofs.
  (match-record config <autofs-configuration>
    (autofs pid-file config-file mounts)
    (list (shepherd-service
           (provision '(autofs))
           (documentation "Run autofs daemon.")
           (requirement '(user-processes loopback))
           (start #~(make-forkexec-constructor
                     (list (string-append #$autofs "/sbin/automount")
                           "-f" "-p" #$pid-file
                           #$(autofs-configuration-file config))
                     #:pid-file #$pid-file))))))

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
