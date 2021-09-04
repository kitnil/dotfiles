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

(define-module (services monitoring)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (monitoring-service
            prometheus-configuration
            prometheus-service-type

            prometheus-alertmanager-configuration
            prometheus-alertmanager-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the monitoring service.
;;;
;;; Code:


;;;
;;; Prometheus
;;;

(define-record-type* <prometheus-configuration>
  prometheus-configuration make-prometheus-configuration
  prometheus-configuration?
  (user           prometheus-configuration-user           ;string
                  (default "prometheus"))
  (group          prometheus-configuration-group          ;string
                  (default "prometheus"))
  (prometheus     prometheus-configuration-prometheus     ;string
                  (default ""))
  (listen-address prometheus-configuration-listen-address ;string
                  (default "0.0.0.0:9090"))
  (config-file    prometheus-configuration-config-file    ;string
                  (default ""))
  (data-path      prometheus-configuration-data-path      ;string
                  (default "/var/lib/prometheus")))

(define (prometheus-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((prometheus-user (prometheus-configuration-user configuration))
        (prometheus-group (prometheus-configuration-group configuration)))
    (list (user-group
           (name prometheus-user)
           (system? #t))
          (user-account
           (name prometheus-user)
           (group prometheus-group)
           (system? #t)
           (comment "prometheus privilege separation user")
           (home-directory (string-append "/var/run/" prometheus-user))
           (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define (prometheus-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (let ((data-path #$(prometheus-configuration-data-path config))
              (user  (getpw #$(prometheus-configuration-user config)))
              (group (getpw #$(prometheus-configuration-group config))))
          (and=> data-path mkdir-p)
          (chown data-path
                 (passwd:uid user)
                 (group:gid group))))))

(define prometheus-shepherd-service
  (match-lambda
    (($ <prometheus-configuration> user group prometheus listen-address config-file data-path)
     (list
      (shepherd-service
       (provision '(prometheus))
       (documentation "Run prometheus.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list #$prometheus
                       (string-append "--web.listen-address=" #$listen-address)
                       (string-append "--config.file=" #$config-file)
                       (string-append "--storage.tsdb.path=" #$data-path))
                 #:user #$user
                 #:group #$group
                 #:log-file "/var/log/prometheus.log"
                 #:environment-variables
                 '("SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                   "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")))
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define prometheus-service-type
  (service-type
   (name 'prometheus)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-shepherd-service)
          (service-extension account-service-type
                             prometheus-account)
          (service-extension activation-service-type
                             prometheus-activation)))
   (default-value (prometheus-configuration))
   (description
    "Run the prometheus.")))


;;;
;;; Prometheus Alertmanager
;;;

(define-record-type* <prometheus-alertmanager-configuration>
  prometheus-alertmanager-configuration make-prometheus-alertmanager-configuration
  prometheus-alertmanager-configuration?
  (user           prometheus-alertmanager-configuration-user           ;string
                  (default "prometheus-alertmanager"))
  (group          prometheus-alertmanager-configuration-group          ;string
                  (default "prometheus-alertmanager"))
  (prometheus-alertmanager   prometheus-alertmanager-configuration-prometheus-alertmanager   ;string
                  (default ""))
  (listen-address prometheus-alertmanager-configuration-listen-address ;string
                  (default "0.0.0.0:9093"))
  (config-file    prometheus-alertmanager-configuration-config-file    ;string
                  (default ""))
  (data-path      prometheus-alertmanager-configuration-data-path      ;string
                  (default "/var/lib/prometheus-alertmanager")))

(define (prometheus-alertmanager-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((prometheus-alertmanager-user
         (prometheus-alertmanager-configuration-user configuration))
        (prometheus-alertmanager-group
         (prometheus-alertmanager-configuration-group configuration)))
    (list (user-group
           (name prometheus-alertmanager-user)
           (system? #t))
          (user-account
           (name prometheus-alertmanager-user)
           (group prometheus-alertmanager-group)
           (system? #t)
           (comment "prometheus-alertmanager privilege separation user")
           (home-directory
            (string-append "/var/run/" prometheus-alertmanager-user))
           (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define (prometheus-alertmanager-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (let ((data-path
               #$(prometheus-alertmanager-configuration-data-path config))
              (user
               (getpw #$(prometheus-alertmanager-configuration-user config)))
              (group
               (getpw #$(prometheus-alertmanager-configuration-group config))))
          (and=> data-path mkdir-p)
          (chown data-path
                 (passwd:uid user)
                 (group:gid group))))))

(define prometheus-alertmanager-shepherd-service
  (match-lambda
    (($ <prometheus-alertmanager-configuration>
        user group prometheus-alertmanager listen-address config-file data-path)
     (list
      (shepherd-service
       (provision '(prometheus-alertmanager))
       (documentation "Run prometheus-alertmanager.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list #$prometheus-alertmanager
                       (string-append "--web.listen-address=" #$listen-address)
                       (string-append "--config.file=" #$config-file)
                       (string-append "--storage.path=" #$data-path))
                 #:user #$user
                 #:group #$group
                 #:log-file "/var/log/prometheus-alertmanager.log"
                 #:environment-variables
                 '("SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                   "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")))
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define prometheus-alertmanager-service-type
  (service-type
   (name 'prometheus-alertmanager)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-alertmanager-shepherd-service)
          (service-extension account-service-type
                             prometheus-alertmanager-account)
          (service-extension activation-service-type
                             prometheus-alertmanager-activation)))
   (default-value (prometheus-alertmanager-configuration))
   (description
    "Run the Prometheus Alertmanager.")))

;;; monitoring.scm ends here
