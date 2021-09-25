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

(define-module (services docker)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages docker)
  #:export (docker-service
            docker-kiwiirc-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the docker service.
;;;
;;; Code:

(define docker-start
  (program-file "docker-start"
                #~(begin
                    (use-modules (ice-9 popen))
                    (let loop ()
                      (let* ((port   (apply open-pipe* OPEN_READ "/run/current-system/profile/bin/docker"
                                            '("ps"))))
                        (if (= (status:exit-val (close-pipe port)) 0)
                            #t
                            (system* "/run/current-system/profile/bin/herd" "start" "dockerd")))
                      (sleep 5)
                      (loop)))))

(define docker-service
  (simple-service 'docker shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(docker-start))
                    (documentation "Run docker.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list #$docker-start)))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))


;;;
;;; kiwiirc
;;;

(define docker-kiwiirc-service
  (simple-service 'docker shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(kiwiirc))
                    (documentation "Run kiwiirc Docker container.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list (string-append #$docker-cli "/bin/docker")
                                    "run"
                                    "--rm"
                                    "--name" "kiwiirc"
                                    "--network=host"
                                    "--volume" "/var/lib/kiwiirc:/kiwiirc-data"
                                    "crashbuggy/kiwiirc:gitasof-21.06.13.1")
                              #:log-file "/var/log/kiwiirc.log"))
                    (respawn? #f)
                    (stop #~(lambda _
                              (invoke #$(program-file "docker-stop-kiwiirc"
                                                      #~(begin
                                                          (system* #$(file-append docker-cli "/bin/docker")
                                                                   "stop" "kiwiirc"))))))))))

;;; docker.scm ends here
