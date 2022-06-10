;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (services docker)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages guile)
  #:use-module (srfi srfi-1)
  #:export (transmission-service

            docker-compose-jackett-service
            jackett-service))

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
                    (requirement '(user-processes loopback))
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


;;;
;;; jackett
;;;

(define docker-compose-jackett-service
  '("jackett"
    ("volumes"
     .
     #("/var/lib/jackett/config:/config"
       "/var/lib/jackett/downloads:/downloads"))
    ("ports" . #("127.0.0.1:9117:9117"))
    ("image" . "lscr.io/linuxserver/jackett:latest")
    ("environment" . #("PUID=1000"
                       "PGID=998"
                       "TZ=Europe/Moscow"
                       "AUTO_UPDATE=true"))))

(define jackett-service
  (service docker-compose-service-type
           (docker-compose-configuration
            (project-name "jackett")
            (compose-file
             (computed-file
              "docker-compose-jackett.json"
              (with-extensions (list guile-json-4)
                (with-imported-modules (source-module-closure '((json builder)))
                  #~(begin
                      (use-modules (json builder))
                      (with-output-to-file #$output
                        (lambda ()
                          (scm->json
                           `(("version" . "2.1")
                             ("services"
                              #$docker-compose-jackett-service)))))))))))))

;;; bittorrent.scm ends here
