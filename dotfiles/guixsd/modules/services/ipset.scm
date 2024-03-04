;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services ipset)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (ipset-configuration
            ipset-configuration?
            ipset-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the ipset service.
;;;
;;; Code:

(define-record-type* <ipset-configuration>
  ipset-configuration make-ipset-configuration
  ipset-configuration?
  (ipset     ipset-ipset                ;<package>
             (default ipset))
  (iptables? ipset-iptables?            ;boolean
             (default #f)))

(define (ipset-program config)
  (program-file
   "ipset-commands"
   #~(begin
       (system* #$(file-append (ipset-ipset config) "/sbin/ipset")
                "-exist" "create" "gov-ru" "hash:ip" "hashsize" "1024" "maxelem" "655360")
       (for-each (lambda (network)
                   (system* #$(file-append ipset "/sbin/ipset")
                            "add" "gov-ru" network))
                 '#$(delete ""
                            (string-split (with-input-from-file "/home/oleg/.local/share/chezmoi/dotfiles/guixsd/etc/ipset/gov-ru.txt"
                                            read-string)
                                          #\newline)))
       (when #$(ipset-iptables? config)
         (system* #$(file-append iptables "/sbin/iptables")
                  "-I" "INPUT"
                  "-m" "set"
                  "--match-set" "gov-ru" "src"
                  "-j" "DROP")))))

(define (ipset-shepherd-service config)
  (list
   (shepherd-service
    (provision '(ipset))
    (documentation "Run ipset.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list #$(ipset-program config))))
    (respawn? #f)
    (stop #~(make-kill-destructor))
    (one-shot? #t))))

(define ipset-service-type
  (service-type
   (name 'ipset)
   (extensions
    (list (service-extension profile-service-type
                             (lambda (config)
                               (list ipset)))
          (service-extension shepherd-root-service-type
                             ipset-shepherd-service)))
   (default-value (ipset-configuration))
   (description "Run the ipset.")))

;;; ipset.scm ends here
