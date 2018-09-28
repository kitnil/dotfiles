;;; GNU Guix --- Functional package management for GNU
;;; Copyright © Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu packages majordomo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim))

(define-public python-cvm
  (let ((source-dir (string-append (getenv "HOME")
                                   "/src/python-cvm")))
    (package
      (name "python-cvm")
      (version (string-append "0.0.1"))
      (source (local-file source-dir
                          #:recursive? #t
                          #:select? (git-predicate source-dir)))
      (build-system python-build-system)
      (arguments
       '(#:tests? #f)) ; no tests
      (propagated-inputs
       `(("python-pexpect" ,python-pexpect)))
      (home-page "https://majordomo.ru/")
      (synopsis "Python interface to @uref{http://billing2.intr}")
      (description
       "This package provides a Python interface to
@uref{http://billing2.intr}")
      (license #f))))

(define-public guile-gms
  (let ((source-dir (string-append (getenv "HOME")
                                   "/src/guile-gms")))
    (package
      (home-page "https://majordomo.ru/")
      (name "guile-gms")
      (version (string-append "0.0.1"))
      (source (local-file source-dir
                          #:recursive? #t
                          #:select? (git-predicate source-dir)))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-26)
                    (ice-9 popen)
                    (ice-9 rdelim))
          #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; Make sure the 'guix' command finds GnuTLS,
               ;; Guile-JSON, and Guile-Git automatically.
               (let* ((out    (assoc-ref outputs "out"))
                      (guile  (assoc-ref inputs "guile"))
                      (gcrypt (assoc-ref inputs "guile-gcrypt"))
                      (gnutls (assoc-ref inputs "gnutls"))
                      (guix   (assoc-ref inputs "guix"))
                      (json   (assoc-ref inputs "guile-json"))
                      (deps   (list gcrypt gnutls guix json out))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (path   (string-join
                               (map (cut string-append <>
                                         "/share/guile/site/"
                                         effective)
                                    deps)
                               ":"))
                      (gopath (string-join
                               (map (cut string-append <>
                                         "/lib/guile/" effective
                                         "/site-ccache")
                                    deps)
                               ":")))

                 (wrap-program (string-append out "/bin/gms")
                   `("GUILE_LOAD_PATH" ":" prefix (,path))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,gopath)))

                 #t)
               (let* ((guile  (assoc-ref inputs "guile"))
                      (effective
                       (read-line
                        (open-pipe* OPEN_READ
                                    (string-append guile "/bin/guile")
                                    "-c" "(display (effective-version))")))
                      (path (cut string-append <>
                                 "/share/guile/site/"
                                 effective
                                 "/gms")))
                 (with-directory-excursion "gms"
                   (copy-file "config.scm.in" "config.scm")
                   (substitute* "config.scm"
                     (("@PACKAGE_NAME@") ,name)
                     (("@PACKAGE_VERSION@") ,version)
                     (("@PACKAGE_URL@") ,home-page)
                     (("@CVM@") (string-append (assoc-ref inputs "cvm")
                                               "/bin/cvm")))
                   (install-file "config.scm"
                                 (path (assoc-ref outputs "out"))))
                 #t))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("cvm" ,python-cvm)
         ("gnutls" ,gnutls)
         ("guile" ,guile-2.2)
         ("guile-gcrypt" ,guile-gcrypt)
         ("guile-json" ,guile-json)
         ("guix" ,guix)))
      (synopsis "Guile interface to Majordomo API")
      (description
       "This package provides a Guile interface to Majordomo API.")
      (license license:gpl3+))))
