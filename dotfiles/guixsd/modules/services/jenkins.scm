;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021, 2022 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (gnu packages java)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (packages jenkins)
  #:export (jenkins-configuration
            jenkins-service-type))

(define-record-type* <jenkins-configuration>
  jenkins-configuration make-jenkins-configuration
  jenkins-configuration?
  (jenkins               jenkins-configuration-jenkins               ;<package>
                         (default jenkins))
  (arguments             jenkins-configuration-arguments             ;list of strings
                         (default '()))
  (java-arguments        jenkins-configuration-java-arguments        ;list of strings
                         (default '()))
  (plugins               jenkins-configuration-plugins               ;list of <package>
                         (default '()))
  (supplementary-groups  jenkins-configuration-supplementary-groups  ;list of strings
                         (default '()))
  (java                  jenkins-configuration-java                  ;<package>
                         (default openjdk11))
  (environment-variables jenkins-configuration-environment-variables ;list of strings
                         (default '())))

(define (jenkins-account config)
  (list (user-account
         (name "jenkins")
         (group "jenkins")
         (system? #t)
         (comment "Jenkins CI daemon user")
         (home-directory "/var/lib/jenkins"))
        (user-group
         (name "jenkins")
         (system? #t))))

(define (jenkins-activation config)
  #~(begin
      (let* ((user (getpw "jenkins"))
             (home (passwd:dir user))
             (uid (passwd:uid user))
             (group (getgrnam "jenkins"))
             (gid (group:gid group))
             (plugins-dir (string-append home "/plugins")))
        (mkdir-p plugins-dir)
        (chown plugins-dir uid gid)
        (for-each (lambda (file)
                    (copy-file file
                               (string-append plugins-dir "/"
                                              (string-drop file
                                                           (string-length "/gnu/store/zznahyxhfkb8ikbg0v92ghv9lx2gpi3s-")))))
                  '#$(jenkins-configuration-plugins config)))))

(define (jenkins-shepherd-service config)
  (list
   (shepherd-service
    (provision '(jenkins))
    (documentation "Run Jenkins continuous integration tool.")
    (requirement '(user-processes loopback))
    (start #~(make-forkexec-constructor
              (list (string-append #$(jenkins-configuration-java config)
                                   "/bin/java")
                    #$@(jenkins-configuration-java-arguments config)
                    "-jar" (string-append #$(jenkins-configuration-jenkins config)
                                          "/webapps/jenkins.war")
                    #$@(jenkins-configuration-arguments config))
              #:user "jenkins"
              #:group "jenkins"
              #:log-file "/var/log/jenkins.log"
              #:environment-variables
              (append (list (string-append "PATH="
                                           (string-append #$git "/bin")
                                           ":" "/run/setuid-programs"
                                           ":" "/run/current-system/profile/bin")
                            "JENKINS_HOME=/var/lib/jenkins"
                            "SSL_CERT_DIR=/run/current-system/profile/etc/ssl/certs"
                            "SSL_CERT_FILE=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                            "GIT_SSL_CAINFO=/run/current-system/profile/etc/ssl/certs/ca-certificates.crt"
                            #$@(jenkins-configuration-environment-variables config))
                      (remove (lambda (str)
                                (or (string-prefix? "PATH=" str)
                                    (string-prefix? "HOME=" str)
                                    (string-prefix? "SSL_CERT_DIR=" str)
                                    (string-prefix? "SSL_CERT_FILE=" str)
                                    (string-prefix? "GIT_SSL_CAINFO=" str)))
                              (environ)))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define jenkins-service-type
  (service-type (name 'jenkins)
                (extensions (list (service-extension shepherd-root-service-type
                                                     jenkins-shepherd-service)
                                  (service-extension account-service-type
                                                     jenkins-account)
                                  (service-extension activation-service-type
                                                     jenkins-activation)))
                (description "Run Jenkins CI.")))
