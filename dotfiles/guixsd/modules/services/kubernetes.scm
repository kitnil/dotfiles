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

(define-module (services kubernetes)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (packages kubernetes)
  #:export (kubernetes-k3s-configuration
            kubernetes-k3s-configuration?
            kubernetes-k3s-service-type

            kubernetes-k3s-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the kubernetes service.
;;;
;;; Code:

(define-record-type* <kubernetes-k3s-configuration>
  kubernetes-k3s-configuration make-kubernetes-k3s-configuration
  kubernetes-k3s-configuration?
  (k3s       kubernetes-k3s-configuration-k3s       ;<package>
             (default k3s))
  (arguments kubernetes-k3s-configuration-arguments ;list of strings
             (default '()))
  (kubevirt? kubernetes-k3s-configuration-kubevirt? ;boolean
             (default #f))
  (log-file  kubernetes-k3s-configuration-log-file  ;string
             (default "/var/log/k3s.log"))
  (runtime   kubernetes-k3s-configuration-runtime   ;symbol
             (default 'docker)))

(define (kubernetes-k3s-log-rotations config)
  (list (log-rotation
         (files (list (kubernetes-k3s-configuration-log-file config))))))

(define (kubernetes-k3s-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (when #$(kubernetes-k3s-configuration-kubevirt? config)
          (invoke (string-append #$util-linux "/bin/mount")
                  "--make-shared" "/"))

        ;; XXX: Fix connection to `/run/k3s/containerd/containerd.sock'.
        ;;
        ;; time="2022-05-21T14:03:02+03:00" level=info msg="Waiting for
        ;; containerd startup: rpc error: code = Unavailable desc = connection
        ;; error: desc = \"transport: Error while dialing dial unix
        ;; /run/k3s/containerd/containerd.sock: connect: connection refused\""
        ;; (when (file-exists? "/run/k3s/containerd")
        ;;   (delete-file-recursively "/run/k3s/containerd"))
        )))

(define (kubernetes-k3s-shepherd-service config)
  (list
   (shepherd-service
    (provision '(kubernetes-k3s))
    (documentation "Run kubernetes-k3s.")
    (requirement (case (kubernetes-k3s-configuration-runtime config)
                   ((docker) '(dockerd))
                   (else '())))
    (start #~(make-forkexec-constructor
              (list #$(file-append (kubernetes-k3s-configuration-k3s config)
                                   "/bin/k3s")
                    "server"
                    #$@(kubernetes-k3s-configuration-arguments config)
                    "--log" #$(kubernetes-k3s-configuration-log-file config))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define kubernetes-k3s-service-type
  (service-type
   (name 'kubernetes-k3s)
   (extensions
    (list (service-extension profile-service-type
                             (lambda (config)
                               (list k3s)))
          (service-extension activation-service-type
                             kubernetes-k3s-activation)
          (service-extension shepherd-root-service-type
                             kubernetes-k3s-shepherd-service)
          (service-extension rottlog-service-type
                             kubernetes-k3s-log-rotations)))
   (default-value '())
   (description "Run the kubernetes-k3s.")))


;;;
;;; Docker
;;;

(define kubernetes-k3s-start
  (program-file "kubernetes-k3s-start"
                #~(begin
                    (use-modules (ice-9 popen))
                    (define count (make-parameter 0))
                    (let loop ()
                      (let ((port (apply open-pipe* OPEN_READ "/run/current-system/profile/bin/k3s"
                                         '("kubectl" "get" "all"))))
                        (if (or (< 15 (count (1+ (count))))
                                (= (status:exit-val (close-pipe port)) 0))
                            #t
                            (begin (system* "/run/current-system/profile/bin/herd" "start" "kubernetes-k3s")
                                   (sleep 5)
                                   (loop))))))))

(define kubernetes-k3s-service
  (simple-service 'kubernetes-k3s shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(kubernetes-k3s-start))
                    (documentation "Run kubernetes-k3s.")
                    (requirement '(dockerd-wait))
                    (start #~(make-forkexec-constructor
                              (list #$kubernetes-k3s-start)))
                    (respawn? #f)
                    (one-shot? #t)
                    (stop #~(make-kill-destructor))))))

;;; kubernetes.scm ends here
