;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services kresd)
  #:use-module (gnu packages dns)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (kresd-service))

(define (kresd-service config)
  (simple-service 'kresd shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(kresd))
                    (auto-start? #t)
                    (requirement '(user-processes loopback))
                    (documentation "Run kresd-daemon.")
                    (start #~(make-forkexec-constructor
                              (list #$(file-append knot-resolver "/sbin/kresd")
                                    "--config" #$config
                                    "--quiet" "-f" "1")
                              #:log-file "/var/log/kresd.log"))
                    (respawn? #t)
                    (stop #~(make-kill-destructor))))))
