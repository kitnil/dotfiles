;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2024 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (wigust packages video)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages chromium)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public y2rss
  (let ((commit "d5bc8173028d1594d6c7575e3f7c309553403074")
        (revision "1"))
    (package
      (name "y2rss")
      (version (git-version "0.0.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AN3223/scripts.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19xlllvgaivaxjc43vxi20cc937pi2vhgsg78iplbrilndy5zssy"))))
      (build-system trivial-build-system)
      (inputs
       `(("python" ,python)))
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (copy-recursively (assoc-ref %build-inputs "source") ".")
           (substitute* "y2rss"
             (("/usr/bin/env python3")
              (string-append (assoc-ref %build-inputs "python")
                             "/bin/python3")))
           (install-file "y2rss" (string-append %output "/bin"))
           #t)))
      (home-page "https://github.com/AN3223/scripts/blob/master/y2rss")
      (synopsis "Convert URL to a YouTube channel/playlist RSS")
      (description "This script takes a URL argument to a YouTube
channel/playlist and returns a link to the corresponding RSS feed.")
      (license #f))))

(define-public ndi
  (package
    (name "ndi")
    (version "5.6.1") ;NDI SDK for Linux/Version.txt
    ;; https://downloads.ndi.tv/SDK/NDI_SDK_Linux/Install_NDI_SDK_v5_Linux.tar.gz
    (source (local-file "/home/oleg/Install_NDI_SDK_v5_Linux.tar.gz"))
    (build-system trivial-build-system)
    (inputs (list bash-minimal tar findutils coreutils gawk gzip tar glibc patchelf `(,gcc "lib") avahi))
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append
                   #$(this-package-input "gzip") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "avahi") "/bin"
                   ":" #$(this-package-input "bash-minimal") "/bin"
                   ":" #$(this-package-input "gawk") "/bin"
                   ":" #$(this-package-input "findutils") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "coreutils") "/bin"
                   ":" #$(this-package-input "patchelf") "/bin"))
          (invoke "tar" "-xf" #$(this-package-native-input "source"))
          (system "echo y | bash -x ./Install_NDI_SDK_v5_Linux.sh")
          ;; Install binaries.
          (mkdir-p (string-append #$output "/bin"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-interpreter"
                              (string-append #$(this-package-input "glibc")
                                             "/lib/ld-linux-x86-64.so.2")
                              (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file)
                                 (string-append #$output "/bin/" file)))
                    '("ndi-benchmark"
                      "ndi-free-audio"
                      "ndi-directory-service"))
          (invoke "patchelf"
                  "--set-interpreter"
                  (string-append #$(this-package-input "glibc")
                                 "/lib/ld-linux-x86-64.so.2")
                  "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                  (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"))
          (copy-file "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"
                     (string-append #$output "/bin/ndi-record"))
          ;; Install libraries.
          (mkdir-p (string-append #$output "/lib"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                              (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file)
                                 (string-append #$output "/lib/" file)))
                    '("libndi.so.5.6.1"))
          (with-directory-excursion (string-append #$output "/lib")
            (for-each (lambda (file)
                        (symlink "libndi.so.5.6.1" file))
                      '("libndi.so.5"
                        "libndi.so")))
          ;; Install misc.
          (for-each (lambda (directory)
                      (mkdir-p (string-append #$output "/" directory))
                      (copy-recursively (string-append "NDI SDK for Linux/" directory)
                                        (string-append #$output "/" directory)))
                    '("include" "examples"))
          (mkdir-p (string-append #$output "/doc"))
          (for-each (lambda (directory)
                      (copy-recursively directory
                                        (string-append #$output "/doc")))
                    '("licenses" "logos")))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public obs-ndi
  (package
    (name "obs-ndi")
    (version "4.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Palakis/obs-ndi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wsb1k0jilcn6gqgpq5kq8hjiwnb6mi2w32fsqgb88iicwj1qa3y"))
              (patches (append (search-patches "hardcode-ndi-path.patch")
                               (search-patches "obs-ndi-add-additional-latency-mode.patch")))))
    (build-system cmake-build-system)
    (inputs
     (list ndi obs qtbase-5))
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'ndi
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((ndi #$(this-package-input "ndi")))
                (substitute* "src/obs-ndi.cpp"
                  (("@NDI@") ndi))
                (delete-file-recursively "lib/ndi")
                (symlink (string-append ndi "/include")
                         "lib/ndi"))))
          (add-after 'install 'obs-plugins
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output "/lib/obs-plugins"))
              (symlink
               (string-append #$output
                              "/obs-plugins/64bit/obs-ndi.so")
               (string-append #$output
                              "/lib/obs-plugins/obs-ndi.so")))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public ndi-4
  (package
    (name "ndi-4")
    (version "4") ;NDI SDK for Linux/Version.txt
    ;; https://downloads.ndi.tv/SDK/NDI_SDK_Linux/Install_NDI_SDK_v5_Linux.tar.gz
    (source (local-file "/home/oleg/src/git.puscii.nl/puppetexp/puppet-sms/files/InstallNDISDK_v4_Linux.sh"))
    (build-system trivial-build-system)
    (inputs (list bash-minimal tar findutils coreutils gawk gzip tar glibc patchelf `(,gcc "lib") avahi))
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH"
                  (string-append
                   #$(this-package-input "gzip") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "avahi") "/bin"
                   ":" #$(this-package-input "bash-minimal") "/bin"
                   ":" #$(this-package-input "gawk") "/bin"
                   ":" #$(this-package-input "findutils") "/bin"
                   ":" #$(this-package-input "tar") "/bin"
                   ":" #$(this-package-input "coreutils") "/bin"
                   ":" #$(this-package-input "patchelf") "/bin"))
          (system (string-append "echo y | bash -x " #$(this-package-native-input "source")))
          ;; Install binaries.
          (mkdir-p (string-append #$output "/bin"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-interpreter"
                              (string-append #$(this-package-input "glibc")
                                             "/lib/ld-linux-x86-64.so.2")
                              (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/" file)
                                 (string-append #$output "/bin/" file)))
                    '("ndi-directory-service"))
          (invoke "patchelf"
                  "--set-interpreter"
                  (string-append #$(this-package-input "glibc")
                                 "/lib/ld-linux-x86-64.so.2")
                  "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                  (string-append "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"))
          (copy-file "NDI SDK for Linux/bin/x86_64-linux-gnu/ndi-record"
                     (string-append #$output "/bin/ndi-record"))
          ;; Install libraries.
          (mkdir-p (string-append #$output "/lib"))
          (for-each (lambda (file)
                      (invoke "patchelf"
                              "--set-rpath" (string-append #$(this-package-input "avahi") "/lib")
                              (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file))
                      (copy-file (string-append "NDI SDK for Linux/lib/x86_64-linux-gnu/" file)
                                 (string-append #$output "/lib/" file)))
                    '("libndi.so.4.0.1"))
          (with-directory-excursion (string-append #$output "/lib")
            (for-each (lambda (file)
                        (symlink "libndi.so.4.0.1" file))
                      '("libndi.so.4"
                        "libndi.so")))
          ;; Install misc.
          (for-each (lambda (directory)
                      (mkdir-p (string-append #$output "/" directory))
                      (copy-recursively (string-append "NDI SDK for Linux/" directory)
                                        (string-append #$output "/" directory)))
                    '("include" "examples"))
          (mkdir-p (string-append #$output "/doc"))
          (for-each (lambda (directory)
                      (copy-recursively directory
                                        (string-append #$output "/doc")))
                    '("licenses" "logos")))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
