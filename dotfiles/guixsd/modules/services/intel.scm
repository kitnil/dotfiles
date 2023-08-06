;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2023 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (services intel)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages bittorrent)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (intel-vaapi-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the Intel
;;; VA-API configuration.
;;;
;;; Code:

(define intel-vappi-configure
  (program-file "intel-vappi-configure"
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (ice-9 popen)
                       (ice-9 rdelim))
          (define (system->string* . args)
            (let* ((port (apply open-pipe* OPEN_READ args))
                   (output (read-string port)))
              (close-pipe port)
              output))
          (define %nix-build
            "/home/oleg/.nix-profile/bin/nix-build")
          (define %nix-channel
            "/home/oleg/.nix-defexpr/channels/nixos-unstable")
          (define %opengl-driver
            "/run/opengl-driver")
          (define vaapi-intel
            (string-trim-right
             (system->string* %nix-build %nix-channel
                              "--no-out-link" "-A" "vaapiIntel")))
          (if (file-exists? "/run/opengl-driver")
              (delete-file "/run/opengl-driver"))
          (symlink vaapi-intel %opengl-driver)))))

(define intel-vaapi-service
  (simple-service 'intel shepherd-root-service-type
                  (list
                   (shepherd-service
                    (provision '(intel-vaapi))
                    (documentation "Configure Intel VA-API")
                    (requirement '(nix))
                    (start #~(make-forkexec-constructor (list #$intel-vappi-configure)))
                    (respawn? #f)
                    (one-shot? #t)
                    (stop #~(make-kill-destructor))))))

;;; intel.scm ends here
