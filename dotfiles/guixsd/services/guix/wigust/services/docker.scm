;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust services docker)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (docker-service
            containerd-service))

(define containerd-service
  (simple-service 'containerd-service shepherd-root-service-type
                  (list
                   (shepherd-service
                    (documentation "containerd daemon.")
                    (provision '(containerd))
                    (start #~(make-forkexec-constructor
                              (list "/home/oleg/.nix-profile/libexec/docker/containerd")
                              #:log-file "/var/log/containerd.log"))
                    (stop #~(make-kill-destructor))))))

(define docker-service
  (simple-service 'docker shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(docker))
                    (auto-start? #f)
                    (documentation "Run docker-daemon.")
                    (requirement '(;containerd
                                   dbus-system
                                   elogind
                                   file-system-/sys/fs/cgroup/blkio
                                   file-system-/sys/fs/cgroup/cpu
                                   file-system-/sys/fs/cgroup/cpuset
                                   file-system-/sys/fs/cgroup/devices
                                   file-system-/sys/fs/cgroup/memory
                                   networking
                                   udev))
                    (start #~(make-forkexec-constructor
                              (list "/home/oleg/bin/run-docker")
                              #:pid-file "/var/run/docker.pid"
                              #:log-file "/var/log/docker.log"))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))
