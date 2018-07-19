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

(define-module (wigust services dns)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (ddclient-service-type
            ddclient-configuration
            opaque-ddclient-configuration))


;;;
;;; ddclient
;;;

(define (uglify-field-name field-name)
  (string-delete #\? (symbol->string field-name)))

(define (serialize-field field-name val)
  (format #t "~a=~a\n" (uglify-field-name field-name) val))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "yes" "no")))

(define (serialize-integer field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val))))

(define (serialize-extra-options extra-options)
  (string-join extra-options "\n" 'suffix))

(define-configuration ddclient-configuration
  (ddclient
   (package ddclient)
   "The ddclient package.")
  (daemon
   (integer 300)
   "The period after which ddclient will retry to check IP and domain name.")
  (syslog
   (boolean #t)
   "Use syslog for the output.")
  (mail
   (string "root")
   "Mail to user.")
  (mail-failure
   (string "root")
   "Mail failed update to user.")
  (pid
   (string "/var/run/ddclient.pid")
   "The ddclient PID file.")
  (ssl
   (boolean #t)
   "Enable SSL support.")
  (extra-options
   (list '())
   "Extra options will be appended to ddclient configuration file."))

(define-configuration opaque-ddclient-configuration
  (ddclient
   (package ddclient)
   "The ddclient package.")
  (ddclient-conf
   (string (configuration-missing-field 'opaque-ddclient-configuration
                                        'ddclient-conf))
   "The contents of the @file{ddclient.conf} to use.")
  (pid
   (string "/var/run/ddclient/ddclient.pid")
   "The ddclient PID file."))

(define (ddclient-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((ddclient-user "ddclient")
        (ddclient-group "ddclient"))
    (list (user-group (name ddclient-group) (system? #t))
          (user-account
           (name ddclient-user)
           (system? #t)
           (group ddclient-group)
           (comment "ddclientd privilege separation user")
           (home-directory (string-append "/var/run/" ddclient-user))
           ;; (shell #~(string-append #$shadow "/sbin/nologin"))
           ))))

(define (ddclient-activation config)
  "Return the activation GEXP for CONFIG."
  (let ((config-str
         (if (opaque-ddclient-configuration? config)
             (opaque-ddclient-configuration-ddclient-conf config)
             (with-output-to-string
               (lambda ()
                 (serialize-configuration config
                                          ddclient-configuration-fields))))))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((dir "/var/cache/ddclient"))
            (mkdir-p dir)
            (chown dir (passwd:uid (getpw "ddclient")) (group:gid (getpw "ddclient"))))
          ;; 'ddclient' complains about ddclient.conf file permissions, which
          ;; rules out /gnu/store.  Thus we copy the ddclient.conf to /etc.
          (mkdir-p "/etc/ddclient")
          (let ((file "/etc/ddclient/ddclient.conf"))
            (copy-file #$(plain-file "ddclient.conf" config-str) file)
            (chmod file #o600)
            (chown file (passwd:uid (getpw "ddclient")) (group:gid (getpw "ddclient"))))))))

(define (ddclient-shepherd-service config)
  "Return a <shepherd-service> for ddclient with CONFIG."
  (let* ((opaque-config? (opaque-ddclient-configuration? config))
         (pid (if opaque-config?
                  (opaque-ddclient-configuration-pid config)
                  (ddclient-configuration-pid config)))
         (ddclient (if opaque-config?
                       (opaque-ddclient-configuration-ddclient config)
                       (ddclient-configuration-ddclient config))))
    (list (shepherd-service
           (provision '(ddclient))
           (documentation "Run ddclient daemon.")
           (start #~(make-forkexec-constructor
                     (list #$(file-append ddclient "/bin/ddclient")
                           "-foreground" "-file" "/etc/ddclient/ddclient.conf"
                           "-debug" "-verbose")
                     #:pid-file #$pid
                     #:environment-variables
                     (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                           "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt")
                     #:user "ddclient"
                     #:group "ddclient"))
           (stop #~(make-kill-destructor))))))

(define ddclient-service-type
  (service-type
   (name 'ddclient)
   (extensions
    (list (service-extension account-service-type ddclient-account)
          (service-extension shepherd-root-service-type ddclient-shepherd-service)
          (service-extension activation-service-type ddclient-activation)))
   (default-value (ddclient-configuration))
   (description "Configure address updating utility for dynamic DNS services,
ddclient.")))

(define (generate-ddclient-documentation)
  (generate-documentation
   `((ddclient-configuration ,ddclient-configuration-fields))
   'ddclient-configuration))

(define (generate-opaque-ddclient-documentation)
  (generate-documentation
   `((opaque-ddclient-configuration ,opaque-ddclient-configuration-fields))
   'opaque-ddclient-configuration))
