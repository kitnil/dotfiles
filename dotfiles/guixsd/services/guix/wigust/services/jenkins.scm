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

(define-module (wigust services jenkins)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages version-control)
  #:export (jenkins-service))

(define jenkins-service
  (simple-service 'jenkins shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(jenkins))
                    (documentation "Run jenkins.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list "/home/oleg/.nix-profile/bin/jenkins"
                                    "--httpListenAddress=127.0.0.1"
                                    "--httpPort=8090"
                                    "--ajp13Port=-1"
                                    "-Djava.awt.headless=true")
                              #:user "oleg"
                              #:group "users"
                              #:supplementary-groups '("docker")
                              #:environment-variables
                              (append (list (string-append "PATH="
                                                           (string-append #$git "/bin")
                                                           ":" "/run/setuid-programs"
                                                           ":" "/run/current-system/profile/bin")
                                            "HOME=/home/oleg"
                                            "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                                            "GIT_SSL_CAINFO=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")
                                      (remove (lambda (str)
                                                (or (string-prefix? "PATH=" str)
                                                    (string-prefix? "HOME=" str)
                                                    (string-prefix? "SSL_CERT_DIR=" str)
                                                    (string-prefix? "SSL_CERT_FILE=" str)
                                                    (string-prefix? "GIT_SSL_CAINFO=" str)))
                                              (environ)))))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))
