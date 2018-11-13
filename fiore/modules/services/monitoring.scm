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

(define-module (services monitoring)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages monitoring)
  #:use-module (guix packages)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (zabbix-server-service-type
            zabbix-agentd-service-type))

(define (uglify-field-name field-name)
  (apply string-append
         (map (lambda (str)
                (if (or (string= str "ssl") (string= str "db"))
                    (string-upcase str)
                    (string-capitalize str)))
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define (serialize-field field-name val)
  #~(format #f "~a=~a~%" #$(uglify-field-name field-name) #$val))

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val ","))))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define-configuration zabbix-server-configuration
  (user
   (string "zabbix")
   "")
  (group
   (string "zabbix")
   "")
  (db-host
   (string "127.0.0.1")
   "")
  (db-name
   (string "zabbix")
   "")
  (db-password
   (string "")
   "")
  (db-port
   (number 5432)
   "")
  (log-file
   (string "/var/log/zabbix/server.log")
   "")
  (pid-file
   (string "/var/run/zabbix/zabbix-server.pid")
   "")
  (ssl-ca-location
   (string "/etc/ssl/certs/ca-certificates.crt")
   "")
  (ssl-cert-location
   (string "/etc/ssl/certs")
   "")
  (extra-options
   (string "")
   ""))

(define-configuration zabbix-agentd-configuration
  (hostname
   (string "Zabbix server")
   "")
  (log-file
   (string "/var/log/zabbix/agentd.log")
   "")
  (pid-file
   (string "/var/run/zabbix/zabbix-agentd.pid")
   "")
  (server
   (list '("127.0.0.1" "192.168.1.0/24" "::1" "2001:db8::/32"))
   "")  
  (server-active
   (list '("127.0.0.1:20051" "zabbix.domain" "[::1]:30051" "::1" "[12fc::1]"))
   "")
  (extra-options
   (string "")
   ""))

(define-configuration opaque-zabbix-server-configuration
  (zabbix-server
   (package zabbix-server)
   "The zabbix-server package.")
  (config
   (string (configuration-missing-field 'opaque-zabbix-server-configuration
                                        'config))
   "The contents of the @code{zabbix-serverrc} to use."))

(define (zabbix-server-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user (zabbix-server-configuration-user config))
        (zabbix-group (zabbix-server-configuration-group config)))
    (list (user-group (name zabbix-group) (system? #t))
          (user-account
           (name zabbix-user)
           (system? #t)
           (group zabbix-group)
           (comment "zabbix privilege separation user")
           (home-directory (string-append "/var/run/" zabbix-user))
           (shell #~(string-append #$shadow "/sbin/nologin"))))))

(define (zabbix-server-activation config)
  "Return the activation gexp for CONFIG."
  ;; (($ <opaque-zabbix-server-configuration> zabbix-server config)
  ;;  (opaque-zabbix-server-configuration-config config))
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpw #$(zabbix-server-configuration-user config)))
            (log-file #$(zabbix-server-configuration-log-file config))
            (pid-file #$(zabbix-server-configuration-pid-file config)))
        (for-each (lambda (file)
                    (let ((directory (dirname file)))
                      (mkdir-p (pk 'directory directory))
                      (chown directory (passwd:uid user) (passwd:gid user))
                      (chmod directory #o755)))
                  (list log-file pid-file "/etc/zabbix/zabbix_server.conf")))
      (copy-file
       #$(mixed-text-file "zabbix_server.conf"
                          (serialize-configuration config zabbix-server-configuration-fields))
       "/etc/zabbix/zabbix_server.conf")))

#;(define (zabbix-server-activation config)
  "Return the activation GEXP for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (let ()
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
                   (list #$(file-append zabbix-server "/sbin/zabbix_server")
                         "--config" "/etc/zabbix/zabbix_server.conf")
                   #:user "zabbix"
                   #:group "zabbix"
                   #:pid-file "/var/run/zabbix/zabbix_server.pid"
                   #:environment-variables
                   (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                         "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt")))
         (stop #~(make-kill-destructor)))))

(define (zabbix-agentd-shepherd-service config)
  "Return a <shepherd-service> for Zabbix agent with CONFIG."
  (list (shepherd-service
         (provision '(zabbix-agentd))
         (documentation "Run zabbix daemon.")
         (start #~(make-forkexec-constructor
                   (list #$(file-append zabbix-server "/sbin/zabbix_agentd")
                         "--config" "/etc/zabbix/zabbix_agentd.conf")
                   #:user "zabbix"
                   #:group "zabbix"
                   #:pid-file "/var/run/zabbix/zabbix_agentd.pid"
                   #:environment-variables
                   (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                         "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt")))
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
   (default-value (zabbix-server-configuration))))

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
