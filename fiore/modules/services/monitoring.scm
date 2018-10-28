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
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (zabbix-server-service-type
            zabbix-agentd-service-type))

(define (uglify-field-name field-name)
  (apply string-append
         (map string-capitalize
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define (serialize-field field-name val)
  (format #t "~a=~a~%" (uglify-field-name field-name) val))

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val))))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define (serialize-boolean field-name val)
  (serialize-string field-name (if val "yes" "no")))
 
(define-configuration zabbix-server-configuration
  (debug-level
   (number 5)
   "")
  (alert-scripts-path
   (string "${datadir}/zabbix/alertscripts")
   "")
  (allow-root?
   (boolean #f)
   "")
  (cache-size
   (string "8M")
   "")
  (cache-update-frequency
   (number 60)
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
  (db-schema
   (string "")
   "")
  (db-socket
   (string "")
   "")
  (external-scripts
   (string "${datadir}/zabbix/externalscripts")
   "")
  (fping-location
   (string "/run/setuid-programs/fping")
   "")
  (fping6-location
   (string "/run/setuid-programs/fping")
   "")
  (history-cache-size
   (string "16M")
   "")
  (history-index-cache-size
   (string "4M")
   "")
  (history-storage-types
   (list '("uint" "dbl" "str" "log" "text"))
   "")
  (history-storage-url
   (string "")
   "")
  (housekeeping-frequency
   (number 1)
   "")
  (include-file
   (list '())
   "")
  (java-gateway
   (string "")
   "")
  (java-gateway-port
   (number 10052)
   "")
  (listen-ip-addresses
   (list '("0.0.0.0" "127.0.0.1"))
   "")
  (listen-port
   (number 10051)
   "")
  (load-module
   (string "")
   "")
  (load-module-path
   (string "${libdir}/modules")
   "")
  (log-file
   (string "")
   "")
  (log-file-size
   (number 1)
   "")
  (log-slow-queries
   (number 3000)
   "")
  (log-type
   (string "file")
   "")
  (max-housekeeper-delete
   (string "5000")
   "")
  (pid-file
   (string "/var/run/zabbix/zabbix-server.pid")
   "")
  (proxy-config-frequency
   (number 3600)
   "")
  (proxy-data-frequency
   (number 1)
   "")
  (snmp-trapper-file
   (string "/tmp/zabbix_traps.tmp")
   "")
  (socket-dir
   (string "/tmp")
   "")
  (source-ip
   (string "")
   "")
  (ssh-key-location
   (string "")
   "")
  (ssl-ca-location
   (string "/etc/ssl/certs/ca-certificates.crt")
   "")
  (ssl-cert-location
   (string "/etc/ssl/certs")
   "")
  (start-alerters
   (number 3)
   "")
  (start-db-syncers
   (number 4)
   "")
  (start-discoverers
   (number 1)
   "")
  (start-escalators
   (number 1)
   "")
  (start-http-pollers
   (number 1)
   "")
  (start-ipmi-pollers
   (number 0)
   "")
  (start-java-pollers
   (number 0)
   "")
  (start-pingers
   (number 1)
   "")
  (start-pollers
   (number 5)
   "")
  (start-pollers-unreachable
   (number 1)
   "")
  (start-preprocessors
   (number 3)
   "")
  (start-proxy-pollers
   (number 1)
   "")
  (start-snmp-trapper
   (string "0")
   "")
  (start-timers
   (number 1)
   "")
  (start-trappers
   (number 5)
   "")
  (start-vmware-collectors
   (number 0)
   "")
  (timeout
   (number 3)
   "")
  (tls-ca-file
   (string "")
   "")
  (tls-cert-file
   (string "")
   "")
  (tls-crl-file
   (string "")
   "")
  (tls-key-file
   (string "")
   "")
  (tmp-dir
   (string "/tmp")
   "")
  (trapper-timeout
   (number 300)
   "")
  (trend-cache-size
   (string "4M")
   "")
  (unavailable-delay
   (number 60)
   "")
  (unreachable-delay
   (number 15)
   "")
  (unreachable-period
   (number 45)
   "")
  (user
   (string "zabbix")
   "")
  (value-cache-size
   (string "8M")
   "")
  (vmware-cache-size
   (string "8M")
   "")
  (vmware-frequency
   (number 60)
   "")
  (vmware-perf-frequency
   (number 60)
   "")
  (vmware-timeout
   (number 10)
   ""))

(define-configuration zabbix-agentd-configuration
  (alias
   (string "")
   "")
  (allow-root?
   (boolean #f)
   "")
  (buffer-send
   (number 5)
   "")
  (buffer-size
   (number 100)
   "")
  (debug-level
   (number 3)
   "")
  (enable-remote-commands?
   (boolean #f)
   "")
  (host-metadata
   (string "")
   "")
  (host-metadata-item
   (string "")
   "")
  (hostname
   (string "Zabbix server")
   "")
  (hostname-item
   (string "system.hostname")
   "")
  (include-files
   (string '())
   "")
  (listen-ip
   (string "0.0.0.0")
   "")
  (listen-port
   (string "10050")
   "")
  (load-module
   (string "")
   "")
  (load-module-path
   (string "${libdir}/modules")
   "")
  (log-file
   (string "/tmp/zabbix-agentd.log")
   "")
  (log-file-size
   (number 1)
   "")
  (log-remote-commands
   (boolean #f)
   "")
  (log-type
   (string "file")
   "")
  (max-lines-per-second
   (number 20)
   "")
  (pid-file
   (string "/var/run/zabbix/zabbix-agentd.pid")
   "")
  (refresh-active-checks
   (number 120)
   "")
  (server
   (string '("127.0.0.1" "192.168.1.0/24" "::1" "2001:db8::/32"))
   "")
  (server-active
   (string "127.0.0.1:20051,zabbix.domain,[::1]:30051,::1,[12fc::1]")
   "")
  (source-ip
   (string "")
   "")
  (start-agents
   (number 3)
   "")
  (timeout
   (number 3)
   "")
  (tls-accept
   (string "unencrypted")
   "")
  (tls-ca-file
   (string "")
   "")
  (tls-cert-file
   (string "")
   "")
  (tls-connect
   (string "unencrypted")
   "")
  (tls-crl-file
   (string "")
   "")
  (tls-key-file
   (string "")
   "")
  (tls-psk-file
   (string "")
   "")
  (tls-psk-identity
   (string "")
   "")
  (tls-server-cert-issuer
   (string "")
   "")
  (tls-server-cert-subject
   (string "")
   "")
  (unsafe-user-parameters
   (string "0")
   "")
  (user
   (string "zabbix")
   "")
  (user-parameter
   (string '())
   ""))

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
