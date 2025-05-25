(define-module (wugi packages linux-modules)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (nonguix licenses)
  #:use-module (srfi srfi-26))

(define-public drbd-module
  (package
    (name "drbd-module")
    (version "9.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pkg.linbit.com/downloads/drbd/9/drbd-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0njhcpry0hr4ibclfv3xva21gzp7vsd876qmnvcyhr6mvc2xmjrc"))))
    (build-system linux-module-build-system)
    (inputs
     `(("bash" ,bash)
       ("coccinelle" ,coccinelle)
       ("flex" ,flex)
       ("python" ,python)))
    (arguments
     (list
      #:make-flags #~(list "--debug" "SPAAS=false")
      #:tests? #f ;there are none.
      #:source-directory "drbd"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-kbuild
            (lambda _
              ;; Patch files to refer to executables in the store or $PATH.
              (substitute* "drbd/Kbuild"
                (("/bin/bash") (which "bash")))
              #t))
          (add-after 'install 'rename-kernel-module
            (lambda _
              (use-modules (guix build utils)
                           (ice-9 string-fun))
              ;; Rename drbd to drbd9 because of modprobe loads drbd module
              ;; provided by the kernel instead of the current package.
              (for-each (lambda (file)
                          (rename-file file
                                       (string-replace-substring (string-replace-substring file
                                                                                           "drbd.ko"
                                                                                           "drbd9.ko")
                                                                 "drbd_transport_tcp.ko"
                                                                 "drbd9_transport_tcp.ko")))
                        (find-files #$output))
              #t)))))
    (home-page "https://github.com/linux-thinkpad/tp_smapi")
    (synopsis
     "Linux Kernel module exposing features of ThinkPad hardware")
    (description
     "This package provides a Linux Kernel module that controls
battery charging of specific ThinkPad laptops.  It also includes an improved
version of the HDAPS driver.  The underlying hardware interfaces are
@acronym{SMAPI, System Management Application Program Interface} and direct
access to the embedded controller.")
    (license #f)))
