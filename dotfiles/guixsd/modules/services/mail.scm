;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages mail)
  #:use-module (gnu services certbot)
  #:use-module (gnu services mail)
  #:use-module (guix gexp)
  #:use-module (packages mail)
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

(define %mail-services
 (list
  (service mail-aliases-service-type '(("wigust" "oleg")
                                       ("admin" "oleg")
                                       ("alertmanager" "oleg")))
  (service exim-service-type
           (exim-configuration
            (package exim-lmtp)
            (config-file (local-file "../../exim.conf"))))
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
  (service certbot-service-type
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
