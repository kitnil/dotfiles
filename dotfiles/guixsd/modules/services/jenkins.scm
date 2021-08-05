;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services jenkins)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages java)
  #:use-module (wigust packages jenkins)
  #:export (jenkins-service))

(define jenkins-service
  (simple-service 'jenkins shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(jenkins))
                    (documentation "Run Jenkins continuous integration tool.")
                    (requirement '(user-processes loopback))
                    (start #~(make-forkexec-constructor
                              (list (string-append #$openjdk11 "/bin/java")
                                    "-Xmx512m"
                                    "-jar" (string-append #$jenkins "/webapps/jenkins.war")
                                    "--httpListenAddress=127.0.0.1"
                                    "--httpPort=8090"
                                    "--ajp13Port=-1"
                                    ;; "-Djava.awt.headless=true"
                                    ;; "-Djenkins.install.runSetupWizard=false"
                                    ;; "-Dorg.jenkinsci.plugins.durabletask.BourneShellScript.LAUNCH_DIAGNOSTICS=true"
                                    ;; "-Dorg.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL=86400"

                                    ;; https://www.jenkins.io/doc/book/security/configuring-content-security-policy/
                                    ;; "-Dhudson.model.DirectoryBrowserSupport.CSP="

                                    ;; Managing Security
                                    ;; <https://www.jenkins.io/doc/book/managing/security/#disable-csrf-checking>
                                    ;;
                                    ;; Upgrading to Jenkins LTS 2.176.x
                                    ;; <https://www.jenkins.io/doc/upgrade-guide/2.176/#SECURITY-626>
                                    ;;
                                    ;; Upgrading to Jenkins LTS 2.222.x
                                    ;; <https://www.jenkins.io/doc/upgrade-guide/2.222/#always-enabled-csrf-protection>

                                    ;; Managing Security
                                    ;; <https://www.jenkins.io/doc/book/managing/security/#caveats>
                                    ;; "-Dhudson.security.csrf.GlobalCrumbIssuerConfiguration.DISABLE_CSRF_PROTECTION=true"
                                    )
                              #:user "oleg"
                              #:group "users"
                              #:supplementary-groups '("docker")
                              #:log-file "/var/log/jenkins.log"
                              #:environment-variables
                              (append (list (string-append "PATH="
                                                           (string-append #$git "/bin")
                                                           ":" "/run/setuid-programs"
                                                           ":" "/run/current-system/profile/bin")
                                            "HOME=/home/oleg"
                                            "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                                            "GIT_SSL_CAINFO=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt")
                                      (remove (lambda (str)
                                                (or (string-prefix? "PATH=" str)
                                                    (string-prefix? "HOME=" str)
                                                    (string-prefix? "SSL_CERT_DIR=" str)
                                                    (string-prefix? "SSL_CERT_FILE=" str)
                                                    (string-prefix? "GIT_SSL_CAINFO=" str)))
                                              (environ)))))
                    (respawn? #f)
                    (stop #~(make-kill-destructor))))))
