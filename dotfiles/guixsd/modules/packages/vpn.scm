;;; GNU Guix --- Functional package management for GNU
;;; Copyright 2022 © Oleg Pykhalov <go.wigust@gmail.com>
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

;; guix build --no-offload -L ../../guixsd/modules -f guix.scm

(define-module (packages vpn)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages vpn)
  #:use-module (guix build-system python)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (packages password-utils))

(define-public openvpn-mintotp
  (package
    (name "openvpn-mintotp")
    (version "0.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://cgit.duckdns.org/git/vpn/openvpn-mintotp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hyd44d7gxjd0gacq5g24xigaarg14axja6bf8qdbfqfa6zrwa0a"))))
    (build-system python-build-system)
    (inputs
     (list openvpn))
    (propagated-inputs
     (list mintotp python-pexpect))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-openvpn-path
            (lambda _
              (substitute* "openvpn_otp.py"
                (("\"openvpn")
                 (string-append "\"" #$(this-package-input "openvpn")
                                "/sbin/openvpn")))))
          (add-after 'install 'symlink-to-sbin
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((sbin (string-append (assoc-ref outputs "out") "/sbin")))
                (mkdir-p sbin)
                (symlink (string-append (assoc-ref outputs "out")
                                        "/bin/openvpn-mintotp")
                         (string-append sbin "/openvpn"))))))))
    (home-page "https://github.com/kitnil/dotfiles")
    (synopsis "OpenVPN with authentication via MinTOTP")
    (description "This package provides a OpenVPN Minimal TOTP generator
 written in Python.")
    (license license:expat)))
