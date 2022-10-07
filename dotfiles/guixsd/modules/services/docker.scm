;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages docker)
  #:export (docker-service
            docker-kiwiirc-service

            docker-compose-configuration
            docker-compose-configuration?
            docker-compose-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the docker service.
;;;
;;; Code:

(define* (dockerd-wait #:optional (start? #f))
  (program-file "dockerd-wait"
                #~(begin
                    (use-modules (ice-9 popen))
                    (define count (make-parameter 0))
                    (let loop ()
                      (let* ((port   (apply open-pipe* OPEN_READ "/run/current-system/profile/bin/docker"
                                            '("ps"))))
                        (if (or (< 15 (count (1+ (count))))
                                (= (status:exit-val (close-pipe port)) 0))
                            #t
                            (begin
                              (when #$start?
                                (system* "/run/current-system/profile/bin/herd" "start" "dockerd"))
                              (sleep 5)
                              (loop))))))))

(define docker-service
  (simple-service 'docker shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(dockerd-wait))
                    (documentation "Run docker.")
                    (requirement '())
                    (start #~(make-forkexec-constructor
                              (list #$(dockerd-wait #t))))
                    (respawn? #f)
                    (one-shot? #t)
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


;;;
;;; docker-compose
;;;

(define-record-type* <docker-compose-configuration>
  docker-compose-configuration make-docker-compose-configuration
  docker-compose-configuration?
  (docker-compose docker-compose-configuration-docker-compose ;<package>
                  (default docker-compose))
  (compose-file docker-compose-configuration-compose-file)    ;<file-like> object
  (project-name docker-compose-configuration-project-name)    ;string
  (requirement docker-compose-configuration-requirement     ;list of symbols
               (default '()))
  (respawn? docker-compose-configuration-respawn? ;boolean
            (default #f)))

(define (docker-compose-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (mkdir-p "/var/log/docker-compose"))))

(define (docker-compose-log-rotations config)
  (list
   (log-rotation
    (files
     (list
      (string-append "/var/log/docker-compose/"
                     (docker-compose-configuration-project-name config)
                     ".log"))))))

(define docker-compose-shepherd-service
  (match-lambda
    (($ <docker-compose-configuration> docker-compose compose-file project-name requirement respawn?)
     (list
      (shepherd-service
       (provision (list (string->symbol (string-append "docker-compose-" project-name))))
       (documentation "Run docker-compose.")
       (requirement (append '(networking containerd dockerd)
                            requirement))
       (start #~(make-forkexec-constructor
                 (list (string-append #$docker-compose "/bin/docker-compose")
                       "--file" #$compose-file
                       "--project-name" #$project-name
                       "up")
                 #:log-file #$(string-append "/var/log/docker-compose/" project-name ".log")
                 #:environment-variables
                 (append (list "SSL_CERT_DIR=/etc/ssl/certs"
                               "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                         (remove (lambda (str)
                                   (or (string-prefix? "HOME=" str)
                                       (string-prefix? "SSL_CERT_DIR=" str)
                                       (string-prefix? "SSL_CERT_FILE=" str)))
                                 (environ)))))
       (respawn? respawn?)
       (stop #~(make-kill-destructor)))))))

(define docker-compose-service-type
  (service-type (name 'docker-compose)
                (extensions (list (service-extension activation-service-type
                                                     docker-compose-activation)
                                  (service-extension shepherd-root-service-type
                                                     docker-compose-shepherd-service)
                                  (service-extension rottlog-service-type
                                                     docker-compose-log-rotations)))
                (description "Run docker-compose.")))

;;; docker.scm ends here
