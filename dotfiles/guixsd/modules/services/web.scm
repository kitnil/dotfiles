;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (services docker)
  #:use-module (gnu packages guile)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (json)
  #:use-module (packages password-utils)
  #:use-module (wigust packages web)
  #:export (homer-service-type
            homer-configuration
            %homer-nginx-configuration-nginx

            vault-configuration
            vault-service-type

            radarr-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the homer service.
;;;
;;; Code:


;;;
;;; Homer
;;;

(define %homer-nginx-configuration-nginx
  (nginx-server-configuration
   (root (file-append homer "/share/homer"))
   (listen '("80"))))

(define (file-object? val)
  (or (file-like? val) (file-name? val)))
(define (serialize-file-object field-name val)
  (serialize-string field-name val))

(define (nginx-server-configuration-list? val)
  (and (list? val) (and-map nginx-server-configuration? val)))
(define (serialize-nginx-server-configuration-list field-name val)
  "")

(define-configuration homer-configuration
  (config-file
   (file-object "homer.scm")
   "Path to a file containing configuration in JSON or YAML format.")
  (nginx
   (nginx-server-configuration-list
    (list %homer-nginx-configuration-nginx))
   "NGINX configuration."))

(define (homer-service-etc config)
  "Return a @file{/etc} entry for an @file{homer/config.yml}."
  `(("homer/config.yml" ,(homer-configuration-config-file config))))

(define homer-service-type
  ;; The /etc/homer service.
  (service-type
   (name 'homer)
   (extensions
    (list (service-extension etc-service-type
                             homer-service-etc)
          (service-extension nginx-service-type
                             homer-configuration-nginx)))
   (default-value '())
   (description
    "Populate the @file{/etc/homer/config.yml} based on the given file object.")))


;;;
;;; Vault
;;;

(define-record-type* <vault-configuration>
  vault-configuration make-vault-configuration
  vault-configuration?
  (vault       vault-configuration-vault ;<package>
               (default vault))
  (user        vault-configuration-user  ;string
               (default "vault"))
  (group       vault-configuration-group ;string
               (default "vault"))
  (config-file vault-configuration-config-file ;<file-like>
               (default #f)))

(define (vault-account configuration)
  ;; Return the user accounts and user groups for CONFIG.
  (let ((vault-user (vault-configuration-user configuration))
        (vault-group (vault-configuration-group configuration)))
    (list (user-group
           (name vault-user)
           (system? #t))
          (user-account
           (name vault-user)
           (group vault-group)
           (system? #t)
           (comment "vault privilege separation user")
           (home-directory "/var/lib/vault")))))

(define (vault-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpw #$(vault-configuration-user config)))
               (group (getpw #$(vault-configuration-group config)))
               (home (passwd:dir user))
               (data (string-append home "/data")))
          (mkdir-p data)
          (chown data
                 (passwd:uid user)
                 (group:gid group))))))

(define (vault-shepherd-service config)
  (list
   (shepherd-service
    (provision '(vault))
    (documentation "Run vault.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$(vault-configuration-vault config)
                                   "/bin/vault")
                    "server"
                    "-config" #$(vault-configuration-config-file config))
              #:directory "/var/lib/vault"
              #:user #$(vault-configuration-user config)
              #:group #$(vault-configuration-group config)
              #:log-file "/var/log/vault.log"))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define vault-service-type
  (service-type
   (name 'vault)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        vault-shepherd-service)
     (service-extension account-service-type
                        vault-account)
     (service-extension activation-service-type
                        vault-activation)))
   (default-value (vault-configuration))
   (description
    "Run the vault.")))


;;;
;;;
;;;

(define heimdall-service
  (service docker-compose-service-type
           (docker-compose-configuration
            (project-name "heimdall")
            (compose-file
             (computed-file
              "docker-compose-heimdall.json"
              (with-extensions (list guile-json-4)
                (with-imported-modules (source-module-closure '((json builder)))
                  #~(begin
                      (use-modules (json builder))
                      (with-output-to-file #$output
                        (lambda ()
                          (scm->json
                           `(("version" . "2.1")
                             ("services"
                              ("heimdall"
                               ("volumes" . #("/var/lib/heimdall:/config"))
                               ("ports" . #("127.0.0.1:8050:80"
                                            "127.0.0.1:8445:443"))
                               ("image" . "lscr.io/linuxserver/heimdall:latest")
                               ("environment" . #("PUID=1000"
                                                  "PGID=998"
                                                  "TZ=Europe/Moscow"))
                               ("container_name" . "heimdall")))))))))))))))


;;;
;;; radarr
;;;

(define radarr-service
  (service docker-compose-service-type
           (docker-compose-configuration
            (project-name "radarr")
            (compose-file
             (computed-file
              "docker-compose-radarr.json"
              (with-extensions (list guile-json-4)
                (with-imported-modules (source-module-closure '((json builder)))
                  #~(begin
                      (use-modules (json builder))
                      (with-output-to-file #$output
                        (lambda ()
                          (scm->json
                           `(("version" . "2.1")
                             ("services"
                              ("radarr"
                               ("volumes"
                                .
                                #("/var/lib/radarr/data:/config"
                                  "/var/lib/radarr/movies:/movies"
                                  "/var/lib/radarr/downloadclient-downloads:/downloads"))
                               ("ports" . #("127.0.0.1:7878:7878"))
                               ("image" . "lscr.io/linuxserver/radarr:latest")
                               ("environment" . #("PUID=1000"
                                                  "PGID=998"
                                                  "TZ=Europe/Moscow"))
                               ("container_name" . "radarr")))))))))))))))

;;; web.scm ends here
