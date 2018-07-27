;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust services monitoring)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (wigust packages monitoring)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (zabbix-server-service-type
            zabbix-agentd-service-type))

(define (zabbix-server-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user "zabbix")
        (zabbix-group "zabbix"))
      (list (user-group (name zabbix-group) (system? #t))
        (user-account
         (name zabbix-user)
         (system? #t)
         (group zabbix-group)
         (comment "zabbix privilege separation user")
         (home-directory (string-append "/var/run/" zabbix-user))
         (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define (zabbix-server-activation config)
  "Return the activation GEXP for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpw "zabbix"))
            (group (getpw "zabbix")))
        (let ((dir "/var/run/zabbix"))
          (mkdir-p dir)
          (chmod dir #o700)
          (chown dir (passwd:uid user) (group:gid group))))))

(define (zabbix-agentd-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user "zabbix")
        (zabbix-group "zabbix"))
      (list (user-group (name zabbix-group) (system? #t))
        (user-account
         (name zabbix-user)
         (system? #t)
         (group zabbix-group)
         (comment "zabbix privilege separation user")
         (home-directory (string-append "/var/run/" zabbix-user))
         (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define (zabbix-agentd-activation config)
  "Return the activation GEXP for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpw "zabbix"))
            (group (getpw "zabbix")))
        (let ((dir "/var/run/zabbix"))
          (mkdir-p dir)
          (chmod dir #o700)
          (chown dir (passwd:uid user) (group:gid group))))))

(define (zabbix-server-shepherd-service config)
  "Return a <shepherd-service> for Zabbix server with CONFIG."
  (list (shepherd-service
         (provision '(zabbix-server))
         (documentation "Run zabbix daemon.")
         (start #~(make-forkexec-constructor
                   (list #$(file-append zabbix "/sbin/zabbix_server")
                         "--config" "/etc/zabbix/zabbix_server.conf")
                   #:user "zabbix"
                   #:group "zabbix"
                   #:pid-file "/var/run/zabbix/zabbix_server.pid"))
         (stop #~(make-kill-destructor)))))

(define (zabbix-agentd-shepherd-service config)
  "Return a <shepherd-service> for Zabbix agent with CONFIG."
  (list (shepherd-service
         (provision '(zabbix-agentd))
         (documentation "Run zabbix daemon.")
         (start #~(make-forkexec-constructor
                   (list #$(file-append zabbix "/sbin/zabbix_agentd")
                         "--config" "/etc/zabbix/zabbix_agentd.conf")
                   #:user "zabbix"
                   #:group "zabbix"
                   #:pid-file "/var/run/zabbix/zabbix_agentd.pid"))
         (stop #~(make-kill-destructor)))))

(define zabbix-server-service-type
  (service-type
   (name 'zabbix-server)
   (extensions
    (list (service-extension shepherd-root-service-type
                             zabbix-server-shepherd-service)
          (service-extension account-service-type
                             zabbix-server-account)
          (service-extension activation-service-type
                             zabbix-server-activation)))
   (default-value '())))

(define zabbix-agentd-service-type
  (service-type
   (name 'zabbix-agentd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             zabbix-agentd-shepherd-service)
          (service-extension account-service-type
                             zabbix-agentd-account)
          (service-extension activation-service-type
                             zabbix-agentd-activation)))
   (default-value '())))
