;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services networking)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (packages admin)
  #:export (gre-configuration
            gre-configuration?
            gre-service-type

            crowdsec-configuration
            crowdsec-service-type

            crowdsec-firewall-bouncer-configuration
            crowdsec-firewall-bouncer-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the networking service.
;;;
;;; Code:

(define-record-type* <gre-configuration>
  gre-configuration make-gre-configuration
  gre-configuration?
  (iproute           gre-configuration-iproute           ;<package>
                     (default iproute))
  (ip-address-local  gre-configuration-ip-address-local  ;string
                     (default #f))
  (ip-address-remote gre-configuration-ip-address-remote ;string
                     (default #f))
  (ip-address        gre-configuration-ip-address        ;string
                     (default #f))
  (interface-name    gre-configuration-interface-name    ;string
                     (default #f))
  (routes            gre-configuration-routes            ;list of strings
                     (default #f)))

(define gre-service-stop ;; is not used in one-shot type
  (program-file "gre-stop"
                #~(begin
                    (let ((ip (string-append #$iproute "/sbin/ip")))
                      (display "Deleting GRE tunnel...\n")))))

(define gre-shepherd-service
  (match-lambda
    (($ <gre-configuration> iproute ip-address-local ip-address-remote ip-address interface-name routes)
     (list
      (shepherd-service
       (provision '(gre))
       (documentation "Run GRE tunnel.")
       (requirement '(loopback))
       (start #~(make-forkexec-constructor
                 (list #$(program-file "gre-start"
                                       #~(begin
                                           (let ((ip (string-append #$iproute "/sbin/ip")))
                                             (display "Creating GRE tunnel...\n")
                                             (apply system* (list ip "tunnel" "add" #$interface-name "mode" "gre" "local" #$ip-address-local "remote" #$ip-address-remote "ttl" "255"))
                                             (apply system* (list ip "addr" "add" #$ip-address "dev" #$interface-name))
                                             (apply system* (list ip "link" "set" #$interface-name "up"))
                                             (for-each (lambda (route)
                                                         (apply system* (append (list ip "route")
                                                                                (string-split route #\space))))
                                                       '#$routes)))))))
       (respawn? #f)
       (one-shot? #t)
       (stop #~(make-forkexec-constructor
                (list #$gre-service-stop))))))))

(define gre-service-type
  (service-type (name 'gre)
                (extensions (list (service-extension shepherd-root-service-type
                                                     gre-shepherd-service)))
                (description "Run GRE tunnel.")
                (default-value '())))


;;;
;;; crowdsec
;;;

(define-record-type* <crowdsec-configuration>
  crowdsec-configuration make-crowdsec-configuration
  crowdsec-configuration?
  (crowdsec              crowdsec-configuration-crowdsec              ;string
                         (default crowdsec))
  (environment-variables crowdsec-configuration-environment-variables ;list of strings
                         (default '()))
  (listen-address        crowdsec-configuration-listen-address
                         (default "127.0.0.1:9101"))
  (user                  crowdsec-configuration-user                  ;string
                         (default "crowdsec"))
  (group                 crowdsec-configuration-group                 ;string
                         (default "root")))

(define (crowdsec-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((crowdsec-user
         (crowdsec-configuration-user configuration))
        (crowdsec-group
         (crowdsec-configuration-group configuration)))
    (list (user-group
           (name crowdsec-user)
           (system? #t))
          (user-account
           (name crowdsec-user)
           (group crowdsec-group)
           (system? #t)
           (comment "crowdsec privilege separation user")
           (home-directory
            (string-append "/var/run/" crowdsec-user))))))

(define (crowdsec-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user
                (getpw #$(crowdsec-configuration-user config)))
               (group
                (getpw #$(crowdsec-configuration-group config))))
          (for-each (lambda (directory)
                      (mkdir-p directory)
                      (chown directory (passwd:uid user) (group:gid group)))
                    '("/var/lib/crowdsec"
                      "/var/lib/crowdsec/data"
                      "/etc/crowdsec/hub"))
          (for-each (lambda (file)
                      (unless (file-exists? file)
                        (call-with-output-file file
                          (lambda (port)
                            (newline port)))
                        (chown file (passwd:uid user) (group:gid group))))
                    '("/var/log/crowdsec.log"
                      "/var/log/crowdsec_api.log"))))))

(define (crowdsec-shepherd-service config)
  (list
   (shepherd-service
    (provision '(crowdsec))
    (documentation "Run crowdsec.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(crowdsec-configuration-crowdsec config)
                                   "/bin/crowdsec"))
              #:directory "/var/run/crowdsec"
              #:user #$(crowdsec-configuration-user config)
              #:group #$(crowdsec-configuration-group config)
              #:environment-variables
              (append (list "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")
                      (remove (lambda (str)
                                (or (string-prefix? "SSL_CERT_DIR=" str)
                                    (string-prefix? "SSL_CERT_FILE=" str)))
                              (environ)))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define crowdsec-service-type
  (service-type
   (name 'crowdsec)
   (extensions
    (list (service-extension shepherd-root-service-type
                             crowdsec-shepherd-service)
          (service-extension account-service-type
                             crowdsec-account)
          (service-extension activation-service-type
                             crowdsec-activation)
          ;; Make sure the 'cscli' command is available.
          (service-extension profile-service-type
                             (lambda (config)
                               (list crowdsec)))))
   (default-value (crowdsec-configuration))
   (description
    "Run the crowdsec.")))


;;;
;;; crowdsec-firewall-bouncer-configuration
;;;

(define-record-type* <crowdsec-firewall-bouncer-configuration>
  crowdsec-firewall-bouncer-configuration make-crowdsec-firewall-bouncer-configuration
  crowdsec-firewall-bouncer-configuration?
  (crowdsec-firewall-bouncer crowdsec-firewall-bouncer-configuration-crowdsec-firewall-bouncer ;<package>
                             (default crowdsec-firewall-bouncer)))

(define (crowdsec-firewall-bouncer-shepherd-service config)
  (list
   (shepherd-service
    (provision '(crowdsec-firewall-bouncer))
    (documentation "Run crowdsec-firewall-bouncer.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(crowdsec-firewall-bouncer-configuration-crowdsec-firewall-bouncer config)
                                   "/bin/crowdsec-firewall-bouncer")
                    "-c" "/etc/crowdsec/crowdsec-firewall-bouncer.yaml")
              #:environment-variables
              (append (list (string-append "PATH=" (string-append #$ipset "/sbin")
                                           ":" (string-append #$iptables "/sbin"))
                            "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")
                      (remove (lambda (str)
                                (or (string-prefix? "PATH=" str)
                                    (string-prefix? "SSL_CERT_DIR=" str)
                                    (string-prefix? "SSL_CERT_FILE=" str)))
                              (environ)))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define crowdsec-firewall-bouncer-service-type
  (service-type
   (name 'crowdsec-firewall-bouncer)
   (extensions
    (list (service-extension shepherd-root-service-type
                             crowdsec-firewall-bouncer-shepherd-service)))
   (default-value (crowdsec-firewall-bouncer-configuration))
   (description
    "Run the crowdsec-firewall-bouncer.")))

;;; networking.scm ends here
