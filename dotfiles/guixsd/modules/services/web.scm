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

(define-module (services web)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (packages web)
  #:use-module (srfi srfi-1)
  #:export (nginx-with-lua-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the web service.
;;;
;;; Code:

(define-record-type* <nginx-with-lua-configuration>
  nginx-with-lua-configuration make-nginx-with-lua-configuration
  nginx-with-lua-configuration?
  (package     nginx-with-lua-configuration-package     ;package
               (default nginx-lua))
  (user-name   nginx-with-lua-configuration-user-name   ;string
               (default "nginx-with-lua"))
  (group-name  nginx-with-lua-configuration-group-name  ;string
               (default "nginx-with-lua")))

(define nginx-with-lua-account
  ;; Return the user accounts and user groups for CONFIG.
  (match-lambda
    (($ <nginx-with-lua-configuration> _ user-name group-name)
     (list (user-group
            (name group-name))
           (user-account
            (name user-name)
            (group group-name)
            (comment "nginx-with-lua privilege separation user")
            (home-directory (string-append "/var/run/" user-name))
            (shell #~(string-append #$shadow "/sbin/nologin")))))))

(define nginx-with-lua-activation
  (match-lambda
    (($ <nginx-with-lua-configuration> _ user-name group-name)
     (with-imported-modules '((guix build utils))
       #~(begin
           (let ((user (getpw #$user-name))
                 (group (getpw #$group-name)))
             (for-each (lambda (dir)
                         (mkdir-p (string-append dir "/" #$user-name))
                         (chown (string-append dir "/" #$user-name)
                                (passwd:uid user)
                                (passwd:gid group)))
                       '("/var/run" "/var/log"))))))))

(define nginx-with-lua-shepherd-service
  (match-lambda
    (($ <nginx-with-lua-configuration> package user-name group-name)
     (list (shepherd-service
            (provision '(nginx-with-lua))
            (documentation "Run NGINX with Lua.")
            (requirement '())
            (start #~(make-forkexec-constructor
                      (list (string-append #$package "/sbin/nginx")
                            "-c" #$(local-file "/home/oleg/.local/share/chezmoi/dotfiles/nginx/nginx.conf")
                            "-p" "/tmp/nginx" "-g" "daemon off;")
                      #:environment-variables
                      (list (format #f "LUA_PATH=~{~a/lib/?.lua;~}"
                                    (list #$lua-resty-core
                                          #$lua-resty-lrucache
                                          #$lua-resty-signal
                                          #$lua-tablepool
                                          #$lua-resty-shell))
                            (format #f "LUA_CPATH=~{~a/lib/lua/?.lua;~}"
                                    (list #$lua-resty-signal)))))
            (respawn? #f)
            (stop #~(make-kill-destructor)))))))

(define nginx-with-lua-service-type
  (service-type (name 'nginx-with-lua)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          nginx-with-lua-shepherd-service)
                       (service-extension account-service-type
                                          nginx-with-lua-account)
                       (service-extension activation-service-type
                                          nginx-with-lua-activation)))
                (default-value (nginx-with-lua-configuration))
                (description
                 "Run the nginx-with-lua.")))

;;; web.scm ends here
