;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services openvpn)
  #:use-module (gnu packages vpn)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (openvpn-configuration
            openvpn-configuration?
            openvpn-service-type))

(define-record-type* <openvpn-configuration>
  openvpn-configuration make-openvpn-configuration
  openvpn-configuration?
  (openvpn openvpn-configuration-openvpn ;<package>
           (default openvpn))
  (config  openvpn-configuration-openvpn ;file-like or string
           (default #f))
  (name    openvpn-configuration-name    ;string
           (default #f)))

(define openvpn-activation
  (match-lambda
    (($ <openvpn-configuration> openvpn config name)
     (with-imported-modules '((guix build utils))
       #~(begin
           (mkdir-p "/etc/openvpn")
           (mkdir-p "/var/log/openvpn")
           (mkdir-p "/var/run/openvpn"))))))

(define openvpn-shepherd-service
  (match-lambda
    (($ <openvpn-configuration> openvpn config name)
     (list
      (shepherd-service
       (provision (list (string->symbol (string-append "openvpn-" name))))
       (auto-start? #t)
       (requirement '(user-processes loopback))
       (documentation "Run OpenVPN client.")
       (start #~(make-forkexec-constructor
                 (list (string-append #$openvpn "/sbin/openvpn") "--config" #$config)
                 #:log-file (string-append "/var/log/openvpn/" #$name ".log")))
       (respawn? #t)
       (stop #~(make-kill-destructor)))))))

(define openvpn-service-type
  (service-type (name 'openvpn)
                (extensions (list (service-extension shepherd-root-service-type
                                                     openvpn-shepherd-service)
                                  (service-extension activation-service-type
                                                     openvpn-activation)))
                (description "Run OpenVPN client.")))

;;; openvpn.scm ends here
