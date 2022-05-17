;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services mail)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages mail)
  #:use-module (gnu services certbot)
  #:use-module (gnu services mail)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (packages mail)
  #:use-module (services certbot)
  #:export (%mail-hosts-file-hosts
            %mail-packages
            %mail-services
            %mail-users))

;;; Commentary:
;;;
;;; This module provides a service definition for the mail service.
;;;
;;; Code:

(define %exim-deploy-hook
  (program-file
   "exim-deploy-hook"
   ;; XXX: Send SIGHUP to exim.
   #~(begin
       (unless (file-exists? "/etc/exim")
         (mkdir "/etc/exim"))
       (let* ((cert-directory (getenv "RENEWED_LINEAGE"))
              (user (getpw "exim"))
              (uid (passwd:uid user))
              (gid (passwd:gid user)))
         (copy-file (string-append cert-directory "/"
                                   (readlink (string-append cert-directory "/fullchain.pem")))
                    "/etc/exim/exim.crt")
         (copy-file (string-append cert-directory "/"
                                   (readlink (string-append cert-directory "/privkey.pem")))
                    "/etc/exim/exim.pem")
         (chown "/etc/exim" uid gid)
         (chown "/etc/exim/exim.crt" uid gid)
         (chown "/etc/exim/exim.pem" uid gid)))))

(define %exim-configuration
  (exim-configuration
   (package exim-lmtp)
   (config-file (local-file "../../exim.conf"))))

(define (exim-wrapper config)
  (let ((wrapper
         (program-file
          "exim-wrapper"
          (with-imported-modules '((guix build utils))
            #~(begin
                (use-modules (guix build utils))
                (apply invoke
                       (append (list (string-append #$((@@ (gnu services mail) exim-configuration-package) %exim-configuration)
                                                    "/bin/exim")
                                     "-C"
                                     #$((@@ (gnu services mail) exim-computed-config-file)
                                        ((@@ (gnu services mail) exim-configuration-package)
                                         %exim-configuration)
                                        ((@@ (gnu services mail) exim-configuration-config-file)
                                         %exim-configuration)))
                               (cdr (command-line)))))))))
    (package
      (inherit exim)
      (name "exim-wrapper")
      (source #f)
      (build-system trivial-build-system)
      (arguments
       `(#:builder
         (begin
           (mkdir %output)
           (mkdir (string-append %output "/bin"))
           (copy-file (assoc-ref %build-inputs "wrapper")
                      (string-append %output "/bin/exim"))
           (chmod (string-append %output "/bin/exim") #o555)
           #t)))
      (inputs `(("wrapper" ,wrapper)))
      (synopsis "")
      (description "")
      (license #f))))

(define exim-wrapper-profile
  (compose list exim-wrapper))

(define exim-service-type
  (service-type
   (name 'exim)
   (extensions
    (list (service-extension shepherd-root-service-type (@@ (gnu services mail) exim-shepherd-service))
          (service-extension account-service-type (const (@@ (gnu services mail) %exim-accounts)))
          (service-extension activation-service-type (@@ (gnu services mail) exim-activation))
          (service-extension profile-service-type exim-wrapper-profile)
          (service-extension mail-aliases-service-type (const '()))))))

(define %dovecot-deploy-hook
  (program-file
   "dovecot-deploy-hook"
   ;; XXX: Send SIGHUP to dovecot.
   #~(begin
       (unless (file-exists? "/etc/dovecot")
         (mkdir "/etc/dovecot"))
       (let* ((cert-directory (getenv "RENEWED_LINEAGE"))
              (user (getpw "dovecot"))
              (uid (passwd:uid user))
              (gid (passwd:gid user)))
         (copy-file (string-append cert-directory "/"
                                   (readlink (string-append cert-directory "/fullchain.pem")))
                    "/etc/dovecot/private/default.pem")
         (copy-file (string-append cert-directory "/"
                                   (readlink (string-append cert-directory "/privkey.pem")))
                    "/etc/dovecot/dovecot.pem")
         (chown "/etc/dovecot" uid gid)
         (chown "/etc/dovecot/private/default.pem" uid gid)
         (chown "/etc/dovecot/dovecot.pem" uid gid)))))

(define (%mail-services listen)
 (list
  (service mail-aliases-service-type '(("wigust" "oleg")
                                       ("admin" "oleg")
                                       ("alertmanager" "oleg")))
  (service exim-service-type %exim-configuration)
  (dovecot-service
   #:config (dovecot-configuration
             (disable-plaintext-auth? #f)
             (protocols
              (list (protocol-configuration (name "imap"))
                    (protocol-configuration (name "lmtp"))))
             (auth-username-format "%n")
             (mail-location
              (string-append "maildir:~/Maildir"
                             ":INBOX=~/Maildir/INBOX"
                             ":LAYOUT=fs"))
             (services
              (list
               (service-configuration
                (kind "auth")
                (listeners
                 (list
                  (unix-listener-configuration
                   (group "exim")
                   (mode "0660")
                   (path "auth-client"))))
                (process-limit 1))
               (service-configuration
                (kind "auth")
                (service-count 0)
                (client-limit 10)
                (process-limit 1)
                (listeners
                 (list (unix-listener-configuration (path "auth-userdb")))))))))
  (service (certbot-service-type-custom-nginx "78.108.82.44")
           (certbot-configuration
            (email "admin@wugi.info")
            (certificates
             (list
              (certificate-configuration
               (domains '("smtp.wugi.info"))
               (deploy-hook %exim-deploy-hook))))))))

(define %mail-users
  (list
   (user-account
    (name "wigust")
    (comment "Oleg Pykhalov")
    (group "users")
    (supplementary-groups '("audio" "video")))

   (user-account
    (name "alertmanager")
    (group "users")
    (system? #t)
    (comment "prometheus-alertmanager privilege separation user")
    (shell #~(string-append #$shadow "/sbin/nologin")))))

(define %mail-hosts-file-hosts
  '("wugi.info"))

(define %mail-packages
  (list dovecot swaks))

;;; mail.scm ends here
