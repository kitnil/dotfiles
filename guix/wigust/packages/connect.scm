;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages connect)
  #:use-module (guix)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages ssh)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cisco
  (let ((commit "36ae61a6004cd8e33252ede7957bd777bcfb8129"))
    (package
      (name "cisco")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://cgit.duckdns.org/git/connect/cisco")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "12raq71alycf9lh0gvkk41m85xrcz9ii0xrzfd75j47r6gj8xgr6"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-pexpect" ,python-pexpect)))
      (home-page "https://wugi.info/")
      (synopsis "Automate connection to Cisco hardware over Telnet")
      (description "This package provides a Python script to automate
connection to Cisco hardware over Telnet.")
      (license license:gpl3+))))

(define-public cisco-interact
  (let ((commit "6c0ad9a683023ebf8cf7474dbdfee5084f4601f1"))
    (package
      (name "cisco-interact")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://cgit.duckdns.org/git/connect/cisco-interact")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "01wrsinrm8807pvgwk1sakn3aqnghkciar3j4snxfq9wnlhhhhrs"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-pexpect" ,python-pexpect)))
      (home-page "https://wugi.info/")
      (synopsis "Automate connection to Cisco hardware over Telnet interactively")
      (description "This package provides a Python script to automate
connection to Cisco hardware over Telnet interactively.")
      (license license:gpl3+))))

(define-public ssh-sudo
  (let ((commit "09567495371612e3338a564d037afd08c165d9e0"))
    (package
      (name "ssh-sudo")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://cgit.duckdns.org/git/connect/ssh-sudo")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bgsj1nkhi73rg8w79hkfv0b9rznz2g7iww6d8iw3y5z5qr5adxb"))))
      (build-system python-build-system)
      (propagated-inputs
       `(("python-paramiko" ,python-paramiko)))
      (home-page "https://wugi.info/")
      (synopsis "Automate connection to servers over SSH")
      (description "This package provides a Python script to automate
connection to servers over SSH.")
      (license license:gpl3+))))

(define-public ssh-aliases
  (let ((commit "047f5d2a35d8f97a49545b21614990f6e01f58d3"))
    (package
      (name "ssh-aliases")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://cgit.duckdns.org/git/connect/ssh-aliases")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dk785dmwifghw3nkydxl3h66cy0wf1l8m3khllcynpj1cqb6g6q"))))
      (build-system trivial-build-system)
      (inputs
       `(("guile" ,guile-3.0)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-append
                    (assoc-ref %build-inputs "guile") "/bin"))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "ssh-aliases/main.scm"
             (("/run/current-system/profile/bin/guile")
              (which "guile")))
           (mkdir-p (string-append %output "/bin"))
           (copy-file "ssh-aliases/main.scm"
                      (string-append %output "/bin/ssh-aliases"))
           #t)))
      (home-page "https://wugi.info/")
      (synopsis "Generate aliases based on SSH known-hosts")
      (description "This package provides a Guile script to generate
aliases based on SSH known-hosts.")
      (license license:gpl3+))))

(define-public connect
  (let ((commit "e09253e837bee3aa66abb52966f75d66664c2de5"))
    (package
      (name "connect")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://cgit.duckdns.org/git/connect/connect")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "126d7lnd61pv7iqkhpvas4zksjym80n17p8y7kr8jpk9xrcyk1ni"))))
      (build-system trivial-build-system)
      (inputs
       `(("guile" ,guile-3.0)
         ("cisco", cisco)
         ("cisco-interact", cisco-interact)
         ("pass" ,password-store)
         ("ssh" ,openssh)
         ("ssh-sudo" ,ssh-sudo)
         ("sshpass" ,sshpass)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (setenv "PATH"
                   (string-append
                    (assoc-ref %build-inputs "guile") "/bin"))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "connect"
             (("/run/current-system/profile/bin/guile")
              (which "guile"))
             (("@PASS@")
              (string-append (assoc-ref %build-inputs "pass") "/bin/pass"))
             (("@SSH@")
              (string-append (assoc-ref %build-inputs "ssh") "/bin/ssh"))
             (("@SSHPASS@")
              (string-append (assoc-ref %build-inputs "sshpass") "/bin/sshpass"))
             (("@SSH_SUDO@")
              (string-append (assoc-ref %build-inputs "ssh-sudo") "/bin/ssh_sudo"))
             (("@CISCO@")
              (string-append (assoc-ref %build-inputs "cisco") "/bin/cisco"))
             (("@CISCO_INTERACT@")
              (string-append (assoc-ref %build-inputs "cisco-interact") "/bin/cisco_interact")))
           (mkdir-p (string-append %output "/bin"))
           (copy-file "connect" (string-append %output "/bin/connect"))
           #t)))
      (home-page "https://wugi.info/")
      (synopsis "Guile script to connect to different hardware")
      (description "This package provides a Guile script to connect to
different hardware.")
      (license license:gpl3+))))
