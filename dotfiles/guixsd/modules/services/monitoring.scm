;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022, 2023 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (gnu packages backup)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (packages monitoring)
  #:export (prometheus-configuration
            prometheus-service-type

            prometheus-alertmanager-configuration
            prometheus-alertmanager-service-type

            prometheus-pushgateway-configuration
            prometheus-pushgateway-service-type

            prometheus-dnsmasq-configuration
            prometheus-dnsmasq-service-type
            
            karma-configuration
            karma-service-type

            prometheus-blackbox-exporter-configuration
            prometheus-blackbox-exporter-service-type

            prometheus-bird-exporter-configuration
            prometheus-bird-exporter-service-type

            prometheus-smartctl-exporter-configuration
            prometheus-smartctl-exporter-service-type

            prometheus-exim-exporter-configuration
            prometheus-exim-exporter-service-type

            prometheus-tp-link-exporter-configuration
            prometheus-tp-link-exporter-service-type

            grafana-configuration
            grafana-service-type

            prometheus-lvm-exporter-configuration
            prometheus-lvm-exporter-service-type

            prometheus-restic-exporter-configuration
            prometheus-restic-exporter-service-type

            fatrace-service-type
            fatrace-configuration

            prometheus-shepherd-exporter-service-type))

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
                  (default "/var/lib/prometheus"))
  (arguments      prometheus-configuration-arguments      ;list of strings
                  (default '())))

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

(define %prometheus-log "/var/log/prometheus.log")

(define %prometheus-log-rotations
  (list (log-rotation
         (files (list %prometheus-log)))))

(define prometheus-shepherd-service
  (match-lambda
    (($ <prometheus-configuration> user group prometheus listen-address config-file data-path arguments)
     (list
      (shepherd-service
       (provision '(prometheus))
       (documentation "Run prometheus.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list #$prometheus
                       (string-append "--web.listen-address=" #$listen-address)
                       (string-append "--config.file=" #$config-file)
                       (string-append "--storage.tsdb.path=" #$data-path)
                       #$@arguments)
                 #:user #$user
                 #:group #$group
                 #:log-file #$%prometheus-log
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
                             prometheus-activation)
          (service-extension rottlog-service-type
                             (const %prometheus-log-rotations))))
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
                  (default "/var/lib/prometheus-alertmanager"))
  (arguments      prometheus-alertmanager-configuration-arguments      ;list of strings
                  (default '())))

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

(define %prometheus-alertmanager-log "/var/log/prometheus-alertmanager.log")

(define %prometheus-alertmanager-log-rotations
  (list (log-rotation
         (files (list %prometheus-alertmanager-log)))))

(define prometheus-alertmanager-shepherd-service
  (match-lambda
    (($ <prometheus-alertmanager-configuration>
        user group prometheus-alertmanager listen-address config-file data-path arguments)
     (list
      (shepherd-service
       (provision '(prometheus-alertmanager))
       (documentation "Run prometheus-alertmanager.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list #$prometheus-alertmanager
                       (string-append "--web.listen-address=" #$listen-address)
                       (string-append "--config.file=" #$config-file)
                       (string-append "--storage.path=" #$data-path)
                       #$@arguments)
                 #:user #$user
                 #:group #$group
                 #:log-file #$%prometheus-alertmanager-log
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
                             prometheus-alertmanager-activation)
          (service-extension rottlog-service-type
                             (const %prometheus-alertmanager-log-rotations))))
   (default-value (prometheus-alertmanager-configuration))
   (description
    "Run the Prometheus Alertmanager.")))


;;;
;;; Prometheus Pushgateway
;;;

(define-record-type* <prometheus-pushgateway-configuration>
  prometheus-pushgateway-configuration make-prometheus-pushgateway-configuration
  prometheus-pushgateway-configuration?
  (user           prometheus-pushgateway-configuration-user           ;string
                  (default "prometheus-pushgateway"))
  (group          prometheus-pushgateway-configuration-group          ;string
                  (default "prometheus-pushgateway"))
  (prometheus-pushgateway   prometheus-pushgateway-configuration-prometheus-pushgateway   ;string
                  (default ""))
  (listen-address prometheus-pushgateway-configuration-listen-address ;string
                  (default "0.0.0.0:9091"))
  (config-file    prometheus-pushgateway-configuration-config-file    ;string
                  (default ""))
  (data-path      prometheus-pushgateway-configuration-data-path      ;string
                  (default "/var/lib/prometheus-pushgateway")))

(define (prometheus-pushgateway-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((prometheus-pushgateway-user
         (prometheus-pushgateway-configuration-user configuration))
        (prometheus-pushgateway-group
         (prometheus-pushgateway-configuration-group configuration)))
    (list (user-group
           (name prometheus-pushgateway-user)
           (system? #t))
          (user-account
           (name prometheus-pushgateway-user)
           (group prometheus-pushgateway-group)
           (system? #t)
           (comment "prometheus-pushgateway privilege separation user")
           (home-directory
            (string-append "/var/run/" prometheus-pushgateway-user))
           (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define (prometheus-pushgateway-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (let ((data-path
               #$(prometheus-pushgateway-configuration-data-path config))
              (user
               (getpw #$(prometheus-pushgateway-configuration-user config)))
              (group
               (getpw #$(prometheus-pushgateway-configuration-group config))))
          (and=> data-path mkdir-p)
          (chown data-path
                 (passwd:uid user)
                 (group:gid group))))))

(define %prometheus-pushgateway-log "/var/log/prometheus-pushgateway.log")

(define %prometheus-pushgateway-log-rotations
  (list (log-rotation
         (files (list %prometheus-pushgateway-log)))))

(define prometheus-pushgateway-shepherd-service
  (match-lambda
    (($ <prometheus-pushgateway-configuration>
        user group prometheus-pushgateway listen-address config-file data-path)
     (list
      (shepherd-service
       (provision '(prometheus-pushgateway))
       (documentation "Run prometheus-pushgateway.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list #$prometheus-pushgateway
                       (string-append "--web.listen-address=" #$listen-address)
                       (string-append "--persistence.file=" #$data-path "/metrics"))
                 #:user #$user
                 #:group #$group
                 #:log-file #$%prometheus-pushgateway-log
                 #:environment-variables
                 '("SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                   "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")))
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define prometheus-pushgateway-service-type
  (service-type
   (name 'prometheus-pushgateway)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-pushgateway-shepherd-service)
          (service-extension account-service-type
                             prometheus-pushgateway-account)
          (service-extension activation-service-type
                             prometheus-pushgateway-activation)
          (service-extension rottlog-service-type
                             (const %prometheus-pushgateway-log-rotations))))
   (default-value (prometheus-pushgateway-configuration))
   (description
    "Run the Prometheus Pushgateway.")))


;;;
;;; Prometheus dnsmasq exporter
;;;

(define-record-type* <prometheus-dnsmasq-configuration>
  prometheus-dnsmasq-configuration make-prometheus-dnsmasq-configuration
  prometheus-dnsmasq-configuration?
  (user               prometheus-dnsmasq-configuration-user               ;string
                      (default "prometheus-dnsmasq"))
  (group              prometheus-dnsmasq-configuration-group              ;string
                      (default "prometheus-dnsmasq"))
  (prometheus-dnsmasq prometheus-dnsmasq-configuration-prometheus-dnsmasq ;string
                      (default ""))
  (listen-address     prometheus-dnsmasq-configuration-listen-address     ;string
                      (default "0.0.0.0:9153"))
  (leases-path        prometheus-dnsmasq-configuration-leases-path        ;string
                      (default "/var/lib/misc/dnsmasq.leases")))

(define (prometheus-dnsmasq-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((prometheus-dnsmasq-user
         (prometheus-dnsmasq-configuration-user configuration))
        (prometheus-dnsmasq-group
         (prometheus-dnsmasq-configuration-group configuration)))
    (list (user-group
           (name prometheus-dnsmasq-user)
           (system? #t))
          (user-account
           (name prometheus-dnsmasq-user)
           (group prometheus-dnsmasq-group)
           (system? #t)
           (comment "prometheus-dnsmasq privilege separation user")
           (home-directory
            (string-append "/var/run/" prometheus-dnsmasq-user))
           (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define %prometheus-dnsmasq-log "/var/log/prometheus-dnsmasq.log")

(define %prometheus-dnsmasq-log-rotations
  (list (log-rotation
         (files (list %prometheus-dnsmasq-log)))))

(define prometheus-dnsmasq-shepherd-service
  (match-lambda
    (($ <prometheus-dnsmasq-configuration>
        user group prometheus-dnsmasq listen-address leases-path)
     (list
      (shepherd-service
       (provision '(prometheus-dnsmasq))
       (documentation "Run prometheus-dnsmasq.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list #$prometheus-dnsmasq
                       (string-append "-listen=" #$listen-address)
                       (string-append "-leases_path=" #$leases-path))
                 #:user #$user
                 #:group #$group
                 #:log-file #$%prometheus-dnsmasq-log))
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define prometheus-dnsmasq-service-type
  (service-type
   (name 'prometheus-dnsmasq)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-dnsmasq-shepherd-service)
          (service-extension account-service-type
                             prometheus-dnsmasq-account)
          (service-extension rottlog-service-type
                             (const %prometheus-dnsmasq-log-rotations))))
   (default-value (prometheus-dnsmasq-configuration))
   (description
    "Run the Prometheus Dnsmasq.")))


;;;
;;; Karma
;;;

(define-record-type* <karma-configuration>
  karma-configuration make-karma-configuration
  karma-configuration?
  (karma       karma-configuration-karma        ;string
               (default karma))
  (config-file karma-configuration-config-file  ;string
               (default #f))
  (arguments   karma-configuration-arguments    ;list of strings
               (default '()))
  (user        karma-configuration-user         ;string
               (default "karma"))
  (group       karma-configuration-group        ;string
               (default "karma")))

(define (karma-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((karma-user (karma-configuration-user configuration))
        (karma-group (karma-configuration-group configuration)))
    (list (user-group
           (name karma-user)
           (system? #t))
          (user-account
           (name karma-user)
           (group karma-group)
           (system? #t)
           (comment "karma privilege separation user")
           (home-directory (string-append "/var/run/" karma-user))
           (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define %karma-log "/var/log/karma.log")

(define %karma-log-rotations
  (list (log-rotation
         (files (list %karma-log)))))

(define (karma-shepherd-service config)
  (list
   (shepherd-service
    (provision '(karma))
    (documentation "Run karma.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(karma-configuration-karma config)
                                   "/bin/karma-linux-amd64")
                    (string-append "--config.file=" #$(karma-configuration-config-file config))
                    #$@(karma-configuration-arguments config))
              #:user #$(karma-configuration-user config)
              #:group #$(karma-configuration-group config)
              #:log-file #$%karma-log))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define karma-service-type
  (service-type
   (name 'karma)
   (extensions
    (list (service-extension shepherd-root-service-type
                             karma-shepherd-service)
          (service-extension account-service-type
                             karma-account)
          (service-extension rottlog-service-type
                             (const %karma-log-rotations))))
   (default-value (karma-configuration))
   (description
    "Run the karma.")))


;;;
;;; prometheus-blackbox-exporter
;;;

(define-record-type* <prometheus-blackbox-exporter-configuration>
  prometheus-blackbox-exporter-configuration make-prometheus-blackbox-exporter-configuration
  prometheus-blackbox-exporter-configuration?
  (prometheus-blackbox-exporter prometheus-blackbox-exporter-configuration-prometheus-blackbox-exporter  ;string
                                (default prometheus-blackbox-exporter))
  (config-file           prometheus-blackbox-exporter-configuration-config-file                          ;string
                         (default #f))
  (arguments             prometheus-blackbox-exporter-configuration-arguments                            ;list of strings
                         (default '()))
  (user                  prometheus-blackbox-exporter-configuration-user                                 ;string
                         (default "prometheus-blackbox-exporter"))
  (group                 prometheus-blackbox-exporter-configuration-group                                ;string
                         (default "prometheus-blackbox-exporter"))
  (listen-address        prometheus-blackbox-exporter-configuration-listen-address                       ;string
                         (default "0.0.0.0:9115"))
  (log-level             prometheus-blackbox-exporter-configuration-log-level                            ;string
                         (default ""))
  (environment-variables prometheus-blackbox-exporter-configuration-environment-variables                ;list of strings
                         (default '())))

(define (prometheus-blackbox-exporter-activation configuration)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p "/run/capability-programs")
        (unless (file-exists? "/run/capability-programs/blackbox_exporter")
          (install-file (string-append #$(prometheus-blackbox-exporter-configuration-prometheus-blackbox-exporter configuration)
                                       "/bin/blackbox_exporter")
                        "/run/capability-programs")
          (invoke #$(file-append libcap "/sbin/setcap") "cap_net_raw+ep"
                  "/run/capability-programs/blackbox_exporter")))))

(define (prometheus-blackbox-exporter-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((prometheus-blackbox-exporter-user
         (prometheus-blackbox-exporter-configuration-user configuration))
        (prometheus-blackbox-exporter-group
         (prometheus-blackbox-exporter-configuration-group configuration)))
    (list (user-group
           (name prometheus-blackbox-exporter-user)
           (system? #t))
          (user-account
           (name prometheus-blackbox-exporter-user)
           (group prometheus-blackbox-exporter-group)
           (system? #t)
           (comment "prometheus-blackbox-exporter privilege separation user")
           (home-directory
            (string-append "/var/run/" prometheus-blackbox-exporter-user))
           (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define %prometheus-blackbox-exporter-log "/var/log/prometheus-blackbox-exporter.log")

(define %prometheus-blackbox-exporter-log-rotations
  (list (log-rotation
         (files (list %prometheus-blackbox-exporter-log)))))

(define (prometheus-blackbox-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-blackbox-exporter))
    (documentation "Run prometheus-blackbox-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              `("/run/capability-programs/blackbox_exporter"
                ,(string-append
                  "--config.file="
                  #$(prometheus-blackbox-exporter-configuration-config-file config))
                ,(string-append
                  "--web.listen-address="
                  #$(prometheus-blackbox-exporter-configuration-listen-address config))
                ,@(let ((log-level
                         #$(prometheus-blackbox-exporter-configuration-log-level config)))
                    (if (string-null? log-level)
                        '()
                        (list (string-append "--log.level=" log-level))))
                ,#$@(prometheus-blackbox-exporter-configuration-arguments config))
              #:user #$(prometheus-blackbox-exporter-configuration-user config)
              #:group #$(prometheus-blackbox-exporter-configuration-group config)
              #:log-file #$%prometheus-blackbox-exporter-log
              #:environment-variables
              (append (list (string-append "PATH="
                                           "/run/setuid-programs"
                                           ":" "/run/current-system/profile/bin")
                            "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                            #$@(prometheus-blackbox-exporter-configuration-environment-variables config))
                      (remove (lambda (str)
                                (or (string-prefix? "PATH=" str)
                                    (string-prefix? "SSL_CERT_DIR=" str)
                                    (string-prefix? "SSL_CERT_FILE=" str)))
                              (environ)))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-blackbox-exporter-service-type
  (service-type
   (name 'prometheus-blackbox-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-blackbox-exporter-shepherd-service)
          (service-extension account-service-type
                             prometheus-blackbox-exporter-account)
          (service-extension activation-service-type
                             prometheus-blackbox-exporter-activation)
          (service-extension rottlog-service-type
                             (const %prometheus-blackbox-exporter-log-rotations))))
   (default-value (prometheus-blackbox-exporter-configuration))
   (description
    "Run the prometheus-blackbox-exporter.")))


;;;
;;; prometheus-bird-exporter
;;;

(define-record-type* <prometheus-bird-exporter-configuration>
  prometheus-bird-exporter-configuration make-prometheus-bird-exporter-configuration
  prometheus-bird-exporter-configuration?
  (prometheus-bird-exporter prometheus-bird-exporter-configuration-prometheus-bird-exporter  ;string
                            (default prometheus-bird-exporter))
  (arguments                prometheus-bird-exporter-configuration-arguments                 ;list of strings
                            (default '())))

(define %prometheus-bird-exporter-log "/var/log/prometheus-bird-exporter.log")

(define %prometheus-bird-exporter-log-rotations
  (list (log-rotation
         (files (list %prometheus-bird-exporter-log)))))

(define (prometheus-bird-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-bird-exporter))
    (documentation "Run prometheus-bird-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              `(,(string-append #$(prometheus-bird-exporter-configuration-prometheus-bird-exporter config)
                                "/bin/bird_exporter")
                ,#$@(prometheus-bird-exporter-configuration-arguments config))
              #:log-file #$%prometheus-bird-exporter-log))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-bird-exporter-service-type
  (service-type
   (name 'prometheus-bird-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-bird-exporter-shepherd-service)
          (service-extension rottlog-service-type
                             (const %prometheus-bird-exporter-log-rotations))))
   (default-value (prometheus-bird-exporter-configuration))
   (description
    "Run the prometheus-bird-exporter.")))


;;;
;;; prometheus-smartctl-exporter
;;;

(define-record-type* <prometheus-smartctl-exporter-configuration>
  prometheus-smartctl-exporter-configuration make-prometheus-smartctl-exporter-configuration
  prometheus-smartctl-exporter-configuration?
  (prometheus-smartctl-exporter prometheus-smartctl-exporter-configuration-prometheus-smartctl-exporter  ;string
                                (default prometheus-smartctl-exporter))
  (arguments                    prometheus-smartctl-exporter-configuration-arguments                     ;list of strings
                                (default '())))

(define %prometheus-smartctl-exporter-log "/var/log/prometheus-smartctl-exporter.log")

(define %prometheus-smartctl-exporter-log-rotations
  (list (log-rotation
         (files (list %prometheus-smartctl-exporter-log)))))

(define (prometheus-smartctl-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-smartctl-exporter))
    (documentation "Run prometheus-smartctl-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              `(,(string-append #$(prometheus-smartctl-exporter-configuration-prometheus-smartctl-exporter config)
                                "/bin/smartctl_exporter")
                ,#$@(prometheus-smartctl-exporter-configuration-arguments config))
              #:log-file #$%prometheus-smartctl-exporter-log))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-smartctl-exporter-service-type
  (service-type
   (name 'prometheus-smartctl-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-smartctl-exporter-shepherd-service)
          (service-extension rottlog-service-type
                             (const %prometheus-smartctl-exporter-log-rotations))))
   (default-value (prometheus-smartctl-exporter-configuration))
   (description
    "Run the prometheus-smartctl-exporter.")))


;;;
;;; prometheus-exim-exporter
;;;

(define-record-type* <prometheus-exim-exporter-configuration>
  prometheus-exim-exporter-configuration make-prometheus-exim-exporter-configuration
  prometheus-exim-exporter-configuration?
  (prometheus-exim-exporter prometheus-exim-exporter-configuration-prometheus-exim-exporter  ;string
                            (default prometheus-exim-exporter))
  (arguments                prometheus-exim-exporter-configuration-arguments                 ;list of strings
                            (default '())))

(define %prometheus-exim-exporter-log "/var/log/prometheus-exim-exporter.log")

(define %prometheus-exim-exporter-log-rotations
  (list (log-rotation
         (files (list %prometheus-exim-exporter-log)))))

(define (prometheus-exim-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-exim-exporter))
    (documentation "Run prometheus-exim-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              `(,(string-append #$(prometheus-exim-exporter-configuration-prometheus-exim-exporter config)
                                "/bin/exim_exporter")
                ,#$@(prometheus-exim-exporter-configuration-arguments config))
              #:log-file #$%prometheus-exim-exporter-log
              #:environment-variables
              (append (list "PATH=/run/current-system/profile/bin") ;for exim binary.
                      (remove (lambda (str)
                                (string-prefix? "PATH=" str))
                              (environ)))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-exim-exporter-service-type
  (service-type
   (name 'prometheus-exim-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-exim-exporter-shepherd-service)
          (service-extension rottlog-service-type
                             (const %prometheus-exim-exporter-log-rotations))))
   (default-value (prometheus-exim-exporter-configuration))
   (description
    "Run the prometheus-exim-exporter.")))


;;;
;;; prometheus-tp-link-exporter
;;;

(define-record-type* <prometheus-tp-link-exporter-configuration>
  prometheus-tp-link-exporter-configuration make-prometheus-tp-link-exporter-configuration
  prometheus-tp-link-exporter-configuration?
  (prometheus-tp-link-exporter prometheus-tp-link-exporter-configuration-prometheus-tp-link-exporter ;string
                               (default prometheus-tp-link-exporter))
  (environment-variables       prometheus-tp-link-exporter-configuration-environment-variables       ;list of strings
                               (default '()))
  (host                        prometheus-tp-link-exporter-configuration-host                        ;string
                               (default ""))
  (listen-address              prometheus-tp-link-exporter-configuration-listen-address
                               (default "0.0.0.0:9101"))
  (user                        prometheus-tp-link-exporter-configuration-user                        ;string
                               (default "prometheus-tp-link-exporter"))
  (group                       prometheus-tp-link-exporter-configuration-group                       ;string
                               (default "prometheus-tp-link-exporter"))
  (ssh                         prometheus-tp-link-exporter-configuration-ssh                         ;<package>
                               (default openssh))
  (known-hosts                 prometheus-tp-link-exporter-configuration-known-hosts                 ;list of strings
                               (default '())))

(define (prometheus-tp-link-exporter-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((prometheus-tp-link-exporter-user
         (prometheus-tp-link-exporter-configuration-user configuration))
        (prometheus-tp-link-exporter-group
         (prometheus-tp-link-exporter-configuration-group configuration)))
    (list (user-group
           (name prometheus-tp-link-exporter-user)
           (system? #t))
          (user-account
           (name prometheus-tp-link-exporter-user)
           (group prometheus-tp-link-exporter-group)
           (system? #t)
           (comment "prometheus-tp-link-exporter privilege separation user")
           (home-directory
            (string-append "/var/run/" prometheus-tp-link-exporter-user))))))

(define (prometheus-tp-link-exporter-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user
                (getpw #$(prometheus-tp-link-exporter-configuration-user config)))
               (group
                (getpw #$(prometheus-tp-link-exporter-configuration-group config)))
               (ssh-directory (string-append (passwd:dir user) "/.ssh"))
               (ssh-config (string-append ssh-directory "/config"))
               (ssh-known-hosts (string-append ssh-directory "/known_hosts")))
          (mkdir-p ssh-directory)
          (chown ssh-directory
                 (passwd:uid user)
                 (group:gid group))
          (chmod ssh-directory #o700)
          (when (file-exists? ssh-config)
            (delete-file ssh-config))
          (when (file-exists? ssh-known-hosts)
            (delete-file ssh-known-hosts))
          (symlink #$(mixed-text-file
                      "config"
                      "\
Host " (prometheus-tp-link-exporter-configuration-host config) "
KexAlgorithms +diffie-hellman-group1-sha1
HostKeyAlgorithms +ssh-rsa
PubkeyAcceptedKeyTypes +ssh-rsa
User admin")
                   ssh-config)
          (symlink #$(plain-file
                      "known_hosts"
                      (string-join (prometheus-tp-link-exporter-configuration-known-hosts config)
                                   "\n"))
                   ssh-known-hosts)))))

(define (prometheus-tp-link-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-tp-link-exporter))
    (documentation "Run prometheus-tp-link-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(prometheus-tp-link-exporter-configuration-prometheus-tp-link-exporter config)
                                   "/bin/prometheus-tp-link-exporter"))
              #:user #$(prometheus-tp-link-exporter-configuration-user config)
              #:group #$(prometheus-tp-link-exporter-configuration-group config)
              #:log-file "/var/log/prometheus-tp-link-exporter.log"
              #:environment-variables
              (append '#$(prometheus-tp-link-exporter-configuration-environment-variables config)
                      (remove (lambda (str)
                                (string-prefix? "PATH=" str))
                              (environ))
                      (list (string-append "PATH="
                                           (string-append #$(prometheus-tp-link-exporter-configuration-ssh config) "/bin")
                                           ":" #$(file-append sshpass "/bin"))
                            (string-append "PROMETHEUS_TP_LINK_EXPORTER_HOST="
                                           #$(prometheus-tp-link-exporter-configuration-host config))
                            (string-append "PROMETHEUS_TP_LINK_EXPORTER_LISTEN_ADDRESS="
                                           #$(prometheus-tp-link-exporter-configuration-listen-address config))))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-tp-link-exporter-service-type
  (service-type
   (name 'prometheus-tp-link-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-tp-link-exporter-shepherd-service)
          (service-extension account-service-type
                             prometheus-tp-link-exporter-account)
          (service-extension activation-service-type
                             prometheus-tp-link-exporter-activation)))
   (default-value (prometheus-tp-link-exporter-configuration))
   (description
    "Run the prometheus-tp-link-exporter.")))


;;;
;;; Grafana
;;;

(define-record-type* <grafana-configuration>
  grafana-configuration make-grafana-configuration
  grafana-configuration?
  (grafana               grafana-configuration-grafana               ;string
                         (default grafana))
  (environment-variables grafana-configuration-environment-variables ;list of strings
                         (default '()))
  (host                  grafana-configuration-host                  ;string
                         (default ""))
  (listen-address        grafana-configuration-listen-address
                         (default ""))
  (user                  grafana-configuration-user                  ;string
                         (default "grafana"))
  (group                 grafana-configuration-group                 ;string
                         (default "grafana")))

(define (grafana-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((grafana-user (grafana-configuration-user configuration))
        (grafana-group (grafana-configuration-group configuration)))
    (list (user-group
           (name grafana-user)
           (system? #t))
          (user-account
           (name grafana-user)
           (group grafana-group)
           (system? #t)
           (comment "grafana privilege separation user")
           (home-directory "/var/lib/grafana")))))

(define (grafana-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpw #$(grafana-configuration-user config)))
               (group (getpw #$(grafana-configuration-group config)))
               (home (passwd:dir user))
               (public (string-append home "/public")))
          (when (file-exists? public)
            (delete-file public))
          (symlink (string-append #$(grafana-configuration-grafana config)
                                  "/public")
                   public)))))

(define %grafana-log "/var/log/grafana.log")

(define %grafana-log-rotations
  (list (log-rotation
         (files (list %grafana-log)))))

(define (grafana-shepherd-service config)
  (list
   (shepherd-service
    (provision '(grafana))
    (documentation "Run grafana.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(grafana-configuration-grafana config)
                                   "/bin/grafana-server"))
              #:directory "/var/lib/grafana"
              #:user #$(grafana-configuration-user config)
              #:group #$(grafana-configuration-group config)
              #:log-file #$%grafana-log))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define grafana-service-type
  (service-type
   (name 'grafana)
   (extensions
    (list (service-extension shepherd-root-service-type
                             grafana-shepherd-service)
          (service-extension account-service-type
                             grafana-account)
          (service-extension activation-service-type
                             grafana-activation)
          (service-extension rottlog-service-type
                             (const %grafana-log-rotations))))
   (default-value (grafana-configuration))
   (description
    "Run the grafana.")))


;;;
;;; prometheus-lvm-exporter
;;;

(define-record-type* <prometheus-lvm-exporter-configuration>
  prometheus-lvm-exporter-configuration make-prometheus-lvm-exporter-configuration
  prometheus-lvm-exporter-configuration?
  (prometheus-lvm-exporter prometheus-lvm-exporter-configuration-prometheus-lvm-exporter ;<package>
                           (default prometheus-lvm-exporter)))

(define (prometheus-lvm-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-lvm-exporter))
    (documentation "Run prometheus-lvm-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(prometheus-lvm-exporter-configuration-prometheus-lvm-exporter config)
                                   "/bin/prometheus-lvm-exporter"))
              #:environment-variables
              (append
               (remove (lambda (str)
                         (string-prefix? "PATH=" str))
                       (environ))
               (list
                (string-append "PATH=" (string-append #$lvm2 "/sbin"))))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-lvm-exporter-service-type
  (service-type
   (name 'prometheus-lvm-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-lvm-exporter-shepherd-service)))
   (default-value (prometheus-lvm-exporter-configuration))
   (description
    "Run the prometheus-lvm-exporter.")))


;;;
;;; prometheus-lvm-exporter
;;;

(define-record-type* <prometheus-restic-exporter-configuration>
  prometheus-restic-exporter-configuration make-prometheus-restic-exporter-configuration
  prometheus-restic-exporter-configuration?
  (name prometheus-restic-exporter-name ;string
        (default #f))
  (prometheus-restic-exporter prometheus-restic-exporter-configuration-prometheus-restic-exporter ;<package>
                              (default prometheus-restic-exporter))
  (environment-variables prometheus-restic-exporter-configuration-environment-variables
                         (default '())))

(define (prometheus-restic-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision
     (list
      (string->symbol
       (string-append "prometheus-restic-exporter-"
                      (prometheus-restic-exporter-name config)))))
    (documentation "Run prometheus-restic-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(prometheus-restic-exporter-configuration-prometheus-restic-exporter config)
                                   "/bin/restic-exporter"))
              #:environment-variables
              (append
               '#$(prometheus-restic-exporter-configuration-environment-variables config)
               (remove (lambda (str)
                         (string-prefix? "PATH=" str))
                       (environ))
               (list
                (string-append "RESTIC_EXPORTER_BIN="
                               (string-append #$restic "/bin/restic"))))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-restic-exporter-service-type
  (service-type
   (name 'prometheus-restic-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-restic-exporter-shepherd-service)))
   (default-value (prometheus-restic-exporter-configuration))
   (description
    "Run the prometheus-restic-exporter.")))


;;;
;;; fatrace
;;;

(define-record-type* <fatrace-configuration>
  fatrace-configuration make-fatrace-configuration
  fatrace-configuration?
  (fatrace   fatrace-configuration-fatrace   ;<package>
             (default fatrace))
  (log-file  fatrace-configuration-log-file  ;string
             (default "/var/log/fatrace.log"))
  (arguments fatrace-configuration-arguments ;list of strings
             (default '()))
  (directory fatrace-configuration-directory ;string
             (default "/")))

(define (fatrace-activation config)
  "Return the activation GEXP for CONFIG."
  (let ((log-file (fatrace-configuration-log-file config)))
    #~(begin
        (when (file-exists? #$log-file)
          (delete-file #$log-file)))))

(define (fatrace-log-rotations config)
  (list (log-rotation
         (files (list (fatrace-configuration-log-file config))))))

(define (fatrace-shepherd-service config)
  (list
   (shepherd-service
    (provision '(fatrace))
    (documentation "Run fatrace.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list
               (string-append #$(fatrace-configuration-fatrace config)
                              "/sbin/fatrace")
               (string-append "--output="
                              #$(fatrace-configuration-log-file config))
               #$@(fatrace-configuration-arguments config))
              #:directory #$(fatrace-configuration-directory config)))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define fatrace-service-type
  (service-type
   (name 'fatrace)
   (extensions
    (list (service-extension shepherd-root-service-type
                             fatrace-shepherd-service)
          (service-extension activation-service-type
                             fatrace-activation)
          (service-extension rottlog-service-type
                             fatrace-log-rotations)))
   (default-value (fatrace-configuration))
   (description
    "Run fatrace.")))


;;;
;;;
;;;

(use-modules (prometheus-shepherd-exporter))

(define (prometheus-shepherd-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-shepherd-exporter))
    (documentation "Run prometheus-shepherd-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list #$prometheus-shepherd-exporter)))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define prometheus-shepherd-exporter-service-type
  (service-type
   (name 'prometheus-shepherd-exporter)
   (extensions
    (list (service-extension shepherd-root-service-type
                             prometheus-shepherd-exporter-shepherd-service)))
   (default-value '())
   (description
    "Run the prometheus-shepherd-exporter.")))

;;; monitoring.scm ends here
