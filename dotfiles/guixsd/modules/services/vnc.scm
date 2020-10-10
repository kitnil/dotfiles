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

(define-module (services vnc)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:export (vnc-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the vnc service.
;;;
;;; Code:

(define vnc-service
  (simple-service 'vnc shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(vncserver1))
                    (documentation "Run vnc.")
                    (requirement '(vncserver-config))
                    (start #~(make-forkexec-constructor
                              (list (string-append #$tigervnc-server "/bin/vncserver")
                                    "-passwd" "/home/oleg/.vnc/passwd"
                                                        "-fg" "-xstartup" "/home/oleg/.vnc/xstartup" ":1")
                              #;(system "/bin/sh" "--login" "-c"
                                     (string-join (list (string-append #$tigervnc-server "/bin/vncserver")
                                                        "-fg" "-xstartup" "/home/oleg/.vnc/xstartup" ":1")))
                              #:log-file "/tmp/vncserver.log"
                              #:user "oleg"
                              #:group "users"
                              #:environment-variables
                              (list (string-append "PATH=" (getenv "PATH")
                                                   ":" (string-append #$coreutils "/bin")
                                                   ":" (string-append #$xauth "/bin"))
                                    "HOME=/home/oleg")))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))

;;; vnc.scm ends here
