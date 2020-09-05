;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services bittorrent)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages bittorrent)
  #:use-module (srfi srfi-1)
  #:export (transmission-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the Transmission
;;; BitTorrent client.
;;;
;;; Code:

(define transmission-service
  (simple-service 'transmission shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(transmission))
                    (documentation "Run transmission.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list (string-append #$transmission "/bin/transmission-daemon")
                                    "--logfile" "/home/oleg/.config/shepherd/transmission.log"
                                    "--foreground")
                              #:user "oleg"
                              #:group "users"
                              #:environment-variables
                              (append (list "HOME=/home/oleg"
                                            "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")
                                      (remove (lambda (str)
                                                (or (string-prefix? "HOME=" str)
                                                    (string-prefix? "SSL_CERT_DIR=" str)
                                                    (string-prefix? "SSL_CERT_FILE=" str)))
                                              (environ)))))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))

;;; bittorrent.scm ends here
