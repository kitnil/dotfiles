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

(define-module (services bird)
  #:use-module (gnu packages networking)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (bird-configuration
            bird-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the bird service.
;;;
;;; Code:

(define-record-type* <bird-configuration>
  bird-configuration make-bird-configuration
  bird-configuration?
  (bird        bird-configuration-bird         ;<package>
               (default bird))
  (arguments   bird-configuration-arguments    ;list of strings
               (default '()))
  (config-file bird-configuration-config-file  ;string
               (default #f))
  (log-file    bird-configuration-log-file     ;string
               (default "/var/log/bird.log")))

(define (bird-shepherd-service config)
  (list
   (shepherd-service
    (provision '(bird))
    (documentation "Run Bird Internet Routing Daemon")
    (requirement '(user-processes networking))
    (start #~(make-forkexec-constructor
              (list (string-append #$(bird-configuration-bird config)
                                   "/sbin/bird")
                    "-c" #$(bird-configuration-config-file config)
                    "-d" ;run in foreground with debug
                    #$@(bird-configuration-arguments config))
              #:log-file #$(bird-configuration-log-file config)))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define (bird-log-rotations config)
  (list (log-rotation
         (files (list (bird-configuration-log-file config))))))

(define bird-service-type
  (service-type (name 'syncthing)
                (extensions (list (service-extension shepherd-root-service-type
                                                     bird-shepherd-service)
                                  (service-extension rottlog-service-type
                                                     bird-log-rotations)))
                (description "Run Bird Internet Routing Daemon.")))

;;; bird.scm ends here
