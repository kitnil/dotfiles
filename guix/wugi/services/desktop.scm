;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2025 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wugi services desktop)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (wugi utils package)
  #:export (seatd-service
            bluetooth-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the desktop service.
;;;
;;; Code:

(define seatd-service
  (simple-service 'seatd shepherd-root-service-type
                  (list
                   (shepherd-service
                     (provision '(seatd))
                     (documentation "Run seatd.")
                     (requirement '())
                     (start #~(make-forkexec-constructor
                               (list #$(file-append seatd "/bin/seatd")
                                     "-u" "oleg"
                                     "-g" "users")))
                     (respawn? #f)
                     (stop #~(make-kill-destructor))))))


;;;
;;; bluetooth
;;;

(define (bluetooth-shepherd-service config)
  "Return a shepherd service for @command{bluetoothd}."
  (shepherd-service
   (provision '(bluetooth))
   (requirement '(user-processes dbus-system udev))
   (documentation "Run the bluetoothd daemon.")
   (start #~(make-forkexec-constructor
             (list
              #$(file-append (package-from-program-file
                              (program-file "bluetoothd"
                                            #~(let ((nsenter #$(file-append util-linux+udev "/bin/nsenter")))
                                                (execl nsenter
                                                       nsenter
                                                       "--net=/rootns/net"
                                                       "--"
                                                       #$(file-append bluez "/libexec/bluetooth/bluetoothd") "--debug" "--nodetach")))
                              "/libexec/bluetooth")
                             "/libexec/bluetooth/bluetoothd"))))
   (stop #~(make-kill-destructor))))

(define bluetooth-service-type
  (service-type
   (name 'bluetooth)
   (extensions
    (list (service-extension (@ (gnu services dbus) dbus-root-service-type)
                             (compose list (@ (gnu services desktop) bluetooth-configuration-bluez)))
          (service-extension (@ (gnu services base) udev-service-type)
                             (compose list (@ (gnu services desktop) bluetooth-configuration-bluez)))
          (service-extension etc-service-type
                             (lambda (config)
                               `(("bluetooth"
                                  ,((@@ (gnu services desktop) bluetooth-directory) config)))))
          (service-extension shepherd-root-service-type
                             (compose list bluetooth-shepherd-service))))
   (default-value ((@ (gnu services desktop) bluetooth-configuration)))
   (description "Run the @command{bluetoothd} daemon, which manages all the
Bluetooth devices and provides a number of D-Bus interfaces.")))

;;; desktop.scm ends here
