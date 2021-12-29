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

(define-module (home services monitoring)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (packages monitoring)
  #:use-module (srfi srfi-1)
  #:export (prometheus-ssh-exporter-configuration
            home-prometheus-ssh-exporter-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the monitoring service.
;;;
;;; Code:


;;;
;;; prometheus-ssh-exporter
;;;

(define-record-type* <prometheus-ssh-exporter-configuration>
  prometheus-ssh-exporter-configuration make-prometheus-ssh-exporter-configuration
  prometheus-ssh-exporter-configuration?
  (home-prometheus-ssh-exporter prometheus-ssh-exporter-configuration-prometheus-ssh-exporter  ;string
                                (default prometheus-ssh-exporter))
  (config-file                  prometheus-ssh-exporter-configuration-config-file                   ;<file-like>
                                (default #f))
  (arguments                    prometheus-ssh-exporter-configuration-arguments                     ;list of strings
                                (default '())))

(define (home-prometheus-ssh-exporter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(prometheus-ssh-exporter))
    (documentation "Run prometheus-ssh-exporter.")
    (requirement '())
    (start #~(make-forkexec-constructor
              `(,(string-append #$(prometheus-ssh-exporter-configuration-prometheus-ssh-exporter config)
                                "/bin/ssh_exporter")
                ,(string-append "--config.file="
                                #$(prometheus-ssh-exporter-configuration-config-file config)))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define home-prometheus-ssh-exporter-service-type
  (service-type
   (name 'prometheus-ssh-exporter)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-prometheus-ssh-exporter-shepherd-service)))
   (default-value (prometheus-ssh-exporter-configuration))
   (description
    "Run the prometheus-ssh-exporter.")))

;;; monitoring.scm ends here
