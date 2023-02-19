;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (home services stumpwm)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (home config)
  #:use-module (gnu packages wm)
  #:export (stumpwm-configuration
            stumpwm-service-type))

(define-record-type* <stumpwm-configuration>
  stumpwm-configuration make-stumpwm-configuration
  stumpwm-configuration?
  (config-files stumpwm-configuration-config-files ;list of files
                '())
  (init-config stumpwm-configuration-init-config
               #f))

(define guix.wugi.info
  '("admin.lisp"
    "android.lisp"
    "audio.lisp"
    "autostart.lisp"
    "backup.lisp"
    "chat.lisp"
    "clipboard.lisp"
    "covid19.lisp"
    "cpu.lisp"
    "desktop-0.lisp"
    "disk.lisp"
    "display-0.lisp"
    "display-1.lisp"
    "docker.lisp"
    "documentation.lisp"
    "emacs.lisp"
    "gaps.lisp"
    "gpg.lisp"
    "vpn.lisp"
    "group-1.lisp"
    "hardware.lisp"
    "imap.lisp"
    "keys.lisp"
    "kubernetes.lisp"
    "kodi.lisp"
    "mail.lisp"
    "mem.lisp"
    "mjru.lisp"
    "mode-line.lisp"
    "mpv.lisp"
    "nav.lisp"
    "notify.lisp"
    "password.lisp"
    "bittorrent.lisp"
    "repl.lisp"
    "rest.lisp"
    "rofi.lisp"
    "screenshoot.lisp"
    "streamlink.lisp"
    "swank.lisp"
    "term.lisp"
    "text-editors.lisp"
    "theme.lisp"
    "time.lisp"
    "trans.lisp"
    "utils.lisp"
    "virtualization.lisp"
    "vnc.lisp"
    "web.lisp"
    "xorg.lisp"
    "youtube-dl.lisp"))

(define (stumpwm-config config)
  (append `((".stumpwm.d/init.lisp"
             ,(computed-file
               "init.lisp"
               #~(begin
                   (call-with-output-file #$output
                     (lambda (port)
                       (for-each (lambda (line)
                                   (write line port)
                                   (newline port))
                                 '#$(stumpwm-configuration-init-config config))))))))
          (map (lambda (file-name)
                 (list (string-append ".stumpwm.d/" file-name)
                       (local-file (string-append %project-directory
                                                  "/dot_stumpwm.d/"
                                                  file-name))))
               (stumpwm-configuration-config-files config))))

(define stumpwm-service-type
  (service-type
   (name 'stumpwm)
   (extensions
    (list (service-extension home-files-service-type
                              stumpwm-config)))
   (description
    "Configure StumpWM window manager.")
   (default-value '())))
