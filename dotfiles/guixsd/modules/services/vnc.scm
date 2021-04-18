;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services vnc)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (gnu packages base)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages linux)
  #:export (vncserver-configuration
            vncserver-configuration?
            vncserver-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the vnc service.
;;;
;;; Code:

(define-record-type* <vncserver-configuration>
  vncserver-configuration make-vncserver-configuration
  vncserver-configuration?
  (vncserver vncserver-configuration-vncserver  ;<package>
             (default tigervnc-server))
  (host-name vncserver-configuration-host-name) ;string
  (display   vncserver-configuration-display)   ;number
  (user      vncserver-configuration-user)      ;string
  (group     vncserver-configuration-group)     ;string
  (directory vncserver-configuration-directory) ;string
  (xstartup  vncserver-configuration-xstartup   ;file-like
             (default #f)))

(define vncserver-shepherd-service
  (match-lambda
    (($ <vncserver-configuration> vncserver host-name display user group directory xstartup)
     (let ((xauthority (string-append "/home/" user "/.Xauthority.vnc"))
           (xdg-runtime-dir (string-append "/run/user/" (number->string (passwd:uid (getpw user)))))
           (path #~(string-append #$(file-append coreutils "/bin")
                                  ":" #$(file-append xauth "/bin")
                                  ":" "/run/current-system/profile/bin"
                                  ":" "/bin")))
       (list
        (shepherd-service
         (provision (list (string->symbol (string-append "vncserver" (number->string display)))))
         (documentation "Run vnc.")
         (start #~(make-forkexec-constructor
                   (list (string-append #$vncserver "/bin/vncserver") "-fg" "-xstartup" #$xstartup
                         (string-append ":" (number->string #$display)))
                   #:log-file (string-append "/var/log/vncserver" (number->string #$display) ".log")
                   #:user #$user
                   #:group #$group
                   #:supplementary-groups '("users" "docker" "kvm" "audio" "video" "wheel")
                   #:directory #$directory
                   #:environment-variables
                   (list (string-append "PATH=" (getenv "PATH") ":" #$path)
                         (string-append "HOME=" #$directory)
                         "SSL_CERT_DIR=/etc/ssl/certs"
                         "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
                         (string-append "XAUTHORITY=" #$xauthority)
                         (string-append "XDG_RUNTIME_DIR=" #$xdg-runtime-dir))))
         (requirement '(user-processes host-name udev))
         (respawn? #f)
         (stop #~(lambda _
                   ;; (invoke "/bin/sh" "-c" (format #f "~s"
                   ;;                                (string-join (list #$(file-append vncserver "/bin/vncserver")
                   ;;                                                   "-kill" (string-append ":" (number->string #$display))))))
                   (invoke #$(program-file "vncserver-stop"
                                           #~(begin
                                               (setenv "PATH" (string-append (getenv "PATH") ":" #$path))
                                               (setenv "HOME=" #$directory)
                                               (setenv "SSL_CERT_DIR" "/etc/ssl/certs")
                                               (setenv "SSL_CERT_FILE" "/etc/ssl/certs/ca-certificates.crt")
                                               (setenv "XAUTHORITY" #$xauthority)
                                               (setenv "XDG_RUNTIME_DIR" #$xdg-runtime-dir)
                                               (system* #$(file-append vncserver "/bin/vncserver")
                                                        "-kill" (string-append ":" (number->string #$display))))))))))))))

(define vncserver-service-type
  (service-type (name 'vncserver)
                (extensions (list (service-extension shepherd-root-service-type
                                                     vncserver-shepherd-service)))
                (description
                 "Provide VNC session using the @command{vncserver} program.")))

;;; vnc.scm ends here
