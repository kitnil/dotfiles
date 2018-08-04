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
  #:use-module (ice-9 rdelim)
  #:export (ddclient-service-type
            ddclient-configuration))


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
   (string "/var/run/ddclient/ddclient.pid")
   "The ddclient PID file.")
  (ssl
   (boolean #t)
   "Enable SSL support.")
  (user
   (string "ddclient")
   "Specifies the user name or ID that is used when running ddclient
program.")
  (group
   (string "ddclient")
   "Group of the user who will run the ddclient program.")
  (secret-file
   (string "/etc/ddclient/secrets.conf")
   "Secret file which will be appended to ddclient.conf file.")
  (extra-options
   (list '())
   "Extra options will be appended to ddclient configuration file."))

(define (ddclient-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((ddclient-user (ddclient-configuration-user config))
        (ddclient-group (ddclient-configuration-group config)))
    (list (user-group
           (name ddclient-group)
           (system? #t))
          (user-account
           (name ddclient-user)
           (system? #t)
           (group ddclient-group)
           (comment "ddclientd privilege separation user")
           (home-directory (string-append "/var/run/" ddclient-user))))))

(define (ddclient-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils)
                           (ice-9 rdelim))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 rdelim))
        (let ((ddclient-user
               #$(passwd:uid (getpw (ddclient-configuration-user config))))
              (ddclient-group
               #$(passwd:gid (getpw (ddclient-configuration-group config))))
              (ddclient-secret-file
               #$(ddclient-configuration-secret-file config)))
          ;; 'ddclient' complains about ddclient.conf file permissions, which
          ;; rules out /gnu/store.  Thus we copy the ddclient.conf to /etc.
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chmod dir #o700)
                      (chown dir ddclient-user ddclient-group))
                    '("/var/cache/ddclient" "/var/run/ddclient"
                      "/etc/ddclient"))
          (with-output-to-file "/etc/ddclient/ddclient.conf"
            (lambda ()
              (display
               (string-append
                "# Generated by 'ddclient-service'.\n\n"
                #$(with-output-to-string
                    (lambda ()
                      (serialize-configuration config
                                               ddclient-configuration-fields)))
                (if (string-null? ddclient-secret-file)
                    ""
                    (format #f "\n\n# Appended from '~a'.\n\n~a"
                            ddclient-secret-file
                            (with-input-from-file ddclient-secret-file
                              read-string)))))))
          (chmod "/etc/ddclient/ddclient.conf" #o600)
          (chown "/etc/ddclient/ddclient.conf"
                 ddclient-user ddclient-group)))))

(define (ddclient-shepherd-service config)
  "Return a <shepherd-service> for ddclient with CONFIG."
  (let ((ddclient (ddclient-configuration-ddclient config))
        (ddclient-pid (ddclient-configuration-pid config))
        (ddclient-user (ddclient-configuration-user config))
        (ddclient-group (ddclient-configuration-group config)))
    (list (shepherd-service
           (provision '(ddclient))
           (documentation "Run ddclient daemon.")
           (start #~(make-forkexec-constructor
                     (list #$(file-append ddclient "/bin/ddclient")
                           "-foreground"
                           "-file" "/etc/ddclient/ddclient.conf")
                     #:pid-file #$ddclient-pid
                     #:environment-variables
                     (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                           "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt")
                     #:user #$ddclient-user
                     #:group #$ddclient-group))
           (stop #~(make-kill-destructor))))))

(define ddclient-service-type
  (service-type
   (name 'ddclient)
   (extensions
    (list (service-extension account-service-type
                             ddclient-account)
          (service-extension shepherd-root-service-type
                             ddclient-shepherd-service)
          (service-extension activation-service-type
                             ddclient-activation)))
   (default-value (ddclient-configuration))
   (description "Configure address updating utility for dynamic DNS services,
ddclient.")))

(define (generate-ddclient-documentation)
  (generate-documentation
   `((ddclient-configuration ,ddclient-configuration-fields))
   'ddclient-configuration))
