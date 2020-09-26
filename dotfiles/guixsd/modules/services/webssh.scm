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

(define-module (services webssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (webssh-configuration
            webssh-configuration?
            webssh-service-type
            %webssh-configuration-nginx))

;;; Commentary:
;;;
;;; This module provides a service definition for the webssh service.
;;;
;;; Code:

(define-record-type* <webssh-configuration>
  webssh-configuration make-webssh-configuration
  webssh-configuration?
  (package     webssh-configuration-package     ;package
               (default webssh))
  (user-name   webssh-configuration-user-name   ;string
               (default "webssh"))
  (group-name  webssh-configuration-group-name  ;string
               (default "webssh"))
  (policy      webssh-configuration-policy      ;symbol
               (default #f))
  (known-hosts webssh-configuration-known-hosts ;list of strings
               (default #f))
  (port        webssh-configuration-port        ;number
               (default #f))
  (address     webssh-configuration-address     ;string
               (default #f))
  (log-file    webssh-configuration-log-file    ;string
               (default "/var/log/webssh.log"))
  (log-level   webssh-configuration-log-level   ;symbol
               (default #f)))

(define %webssh-configuration-nginx
  (nginx-server-configuration
   (listen '("80"))
   (locations
    (list (nginx-location-configuration
           (uri "/")
           (body '("proxy_pass http://127.0.0.1:8888;"
                   "proxy_http_version 1.1;"
                   "proxy_read_timeout 300;"
                   "proxy_set_header Upgrade $http_upgrade;"
                   "proxy_set_header Connection \"upgrade\";"
                   "proxy_set_header Host $http_host;"
                   "proxy_set_header X-Real-IP $remote_addr;"
                   "proxy_set_header X-Real-PORT $remote_port;")))))))

(define webssh-account
  ;; Return the user accounts and user groups for CONFIG.
  (match-lambda
    (($ <webssh-configuration> _ user-name group-name _ _ _ _ _ _)
     (list (user-group
            (name group-name))
           (user-account
            (name user-name)
            (group group-name)
            (comment "webssh privilege separation user")
            (home-directory (string-append "/var/run/" user-name))
            (shell #~(string-append #$shadow "/sbin/nologin")))))))

(define webssh-activation
  ;; Return the activation GEXP for CONFIG.
  (match-lambda
    (($ <webssh-configuration> _ user-name group-name policy known-hosts _ _
                               log-file _)
     (with-imported-modules '((guix build utils))
       #~(begin
           (let* ((home-dir (string-append "/var/run/" #$user-name))
                  (ssh-dir (string-append home-dir "/.ssh"))
                  (known-hosts-file (string-append ssh-dir "/known_hosts")))
             (call-with-output-file #$log-file (const #t))
             (mkdir-p ssh-dir)
             (case '#$policy
               ((reject)
                (if '#$known-hosts
                    (call-with-output-file known-hosts-file
                      (lambda (port)
                        (for-each (lambda (host) (display host port) (newline port))
                                  '#$known-hosts)))
                    (display-hint (G_ "webssh: reject policy requires `known-hosts'.")))))
             (for-each (lambda (file)
                         (chown file
                                (passwd:uid (getpw #$user-name))
                                (group:gid (getpw #$group-name))))
                       (list #$log-file ssh-dir known-hosts-file))
             (chmod ssh-dir #o700)))))))

(define webssh-shepherd-service
  (match-lambda
    (($ <webssh-configuration> package user-name group-name policy _ port
                               address log-file log-level)
     (list (shepherd-service
            (provision '(webssh))
            (documentation "Run webssh daemon.")
            (start #~(make-forkexec-constructor
                      `(,(string-append #$webssh "/bin/wssh")
                        ,(string-append "--log-file-prefix=" #$log-file)
                        ,@(case '#$log-level
                            ((debug) '("--logging=debug"))
                            (else '()))
                        ,@(case '#$policy
                            ((reject) '("--policy=reject"))
                            (else '()))
                        ,@(if #$port
                              (list (string-append "--port=" (number->string #$port)))
                              '())
                        ,@(if #$address
                              (list (string-append "--address=" #$address))
                              '()))
                      #:user #$user-name
                      #:group #$group-name))
            (stop #~(make-kill-destructor)))))))

(define webssh-service-type
  (service-type
   (name 'webssh)
   (extensions
    (list (service-extension shepherd-root-service-type
                             webssh-shepherd-service)
          (service-extension account-service-type
                             webssh-account)
          (service-extension activation-service-type
                             webssh-activation)))
   (default-value (webssh-configuration))
   (description
    "Run the webssh.")))

;;; webssh.scm ends here
