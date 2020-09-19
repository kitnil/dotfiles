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

(define-module (services webhook)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages bittorrent)
  #:use-module (srfi srfi-1)
  #:export (webhook-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the webhook.
;;;
;;; Code:

(define webhook-service
  (simple-service 'webhook shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(webhook))
                    (documentation "Run webhook.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list "/home/oleg/.nix-profile/bin/webhook"
                                    "-hooks" #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/webhook/hooks.json")
                                    "-port" "9090")
                              #:user "oleg"
                              #:group "users"
                              #:environment-variables
                              (append (list "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                                            (string-append "PATH=" "/home/oleg/.guix-profile/bin"
                                                           ":" "/run/setuid-programs"
                                                           ":" "/run/current-system/profile/bin")
                                            "DISPLAY=:0.0")
                                      (environ))))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))

;;; webhook.scm ends here
